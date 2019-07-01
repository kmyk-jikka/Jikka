open System
open System.Text
open FParsec

type VarName
    = VarName of string

type ConstName
    = ConstName of string

type IntValue
    = Int of int64
    | IntVar of VarName
    | IntConst of ConstName
    | Subscript of IntArray * IntValue
    | Add of IntValue * IntValue
    | Sub of IntValue * IntValue
    | Mul of IntValue * IntValue
    | Div of IntValue * IntValue
    | Mod of IntValue * IntValue
    | Sum of VarName * IntPred * IntValue
    | Max of VarName * IntPred * IntValue
    | Min of VarName * IntPred * IntValue

and IntArray
    = IntArrayConst of ConstName

and IntSet
    = Range of IntValue * IntValue

and IntPred
    = In of IntSet
    | Lt of IntValue


let pvarname = pipe2 lower (many letter) (fun c s -> VarName (System.String.Concat (c :: s)))
let pconstname = pipe2 upper (many letter) (fun c s -> ConstName (System.String.Concat (c :: s)))

let pparen l r body = between (pchar l >>. spaces) (spaces .>> pchar r) body
let pcamma () = spaces >>. pchar ',' >>. spaces
let punderscore a i cont = pipe2 (a .>> spaces .>> pchar '_' .>> spaces) i cont
let pintegral name pred body cont = pipe2 (punderscore (pstring name) (pparen '{' '}' pred .>> spaces) (fun a i -> i)) body cont

let pintarray =
    pconstname |>> IntArrayConst

let pintatomic =
    choice
        [ pint64 |>> Int
        ; pvarname |>> IntVar
        ; pconstname |>> IntConst
        ]

let rec pintset () =
    pparen '[' ')' (pipe2 (parse.Delay pintvalue .>> pcamma ()) (parse.Delay pintvalue) (fun l r -> Range (l, r)))

and pintpred () =
    tuple2
        (pvarname .>> spaces)
        (choice
            [ pchar '<' >>. spaces >>. parse.Delay pintvalue |>> Lt
            ; pstring "\\in" >>. spaces >>. parse.Delay pintset |>> In
            ])

and pintvalue () =
    let opp = new OperatorPrecedenceParser<IntValue, option<VarName * IntPred>, unit>()
    opp.TermParser <- choice
        [ pparen '(' ')' opp.ExpressionParser
        ; attempt (punderscore pintarray (parse.Delay pintvalue) (fun a i -> Subscript (a, i)))
        ; pintatomic
        ] .>> spaces
    let infix a c e = InfixOperator<IntValue, option<VarName * IntPred>, unit> (a, spaces >>. preturn None, c, Associativity.Left , e)
    opp.AddOperator (infix "+" 2 (fun x y -> Add (x, y)))
    opp.AddOperator (infix "-" 2 (fun x y -> Sub (x, y)))
    opp.AddOperator (infix "\\cdot" 3 (fun x y -> Mul (x, y)))
    opp.AddOperator (infix "/" 3 (fun x y -> Div(x, y)))
    opp.AddOperator (infix "\\bmod" 3 (fun x y -> Mod (x, y)))
    opp.AddOperator (infix "*" 3 (fun x y -> Mul (x, y)))
    opp.AddOperator (infix "%" 3 (fun x y -> Mod (x, y)))
    let pred = spaces >>. pchar '_' >>. spaces >>. pparen '{' '}' (parse.Delay pintpred |>> Some) .>> spaces
    let prefix a c f = PrefixOperator<IntValue, option<VarName * IntPred>, unit> (a, pred, c, true, (), fun x y -> f (Option.get x) y)
    opp.AddOperator (prefix "\\sum" 1 (fun (name, pred) body -> Sum (name, pred, body)))
    opp.AddOperator (prefix "\\min" 1 (fun (name, pred) body -> Min (name, pred, body)))
    opp.AddOperator (prefix "\\max" 1 (fun (name, pred) body -> Max (name, pred, body)))
    opp.ExpressionParser

let parser : Parser<IntValue, unit> = spaces >>. pintvalue () .>> spaces .>> eof


type Type
    = IntType
    | IntArrayType

let freevars ast =
    let rec intvalue ast acc =
        match ast with
        | Int _ -> acc
        | IntVar _ -> acc
        | IntConst (ConstName name) -> (name, IntType) :: acc
        | Subscript (x, y) -> intarray x (intvalue y acc)
        | Add (x, y) -> intvalue x (intvalue y acc)
        | Sub (x, y) -> intvalue x (intvalue y acc)
        | Mul (x, y) -> intvalue x (intvalue y acc)
        | Div (x, y) -> intvalue x (intvalue y acc)
        | Mod (x, y) -> intvalue x (intvalue y acc)
        | Sum (_, y, z) -> intpred y (intvalue z acc)
        | Max (_, y, z) -> intpred y (intvalue z acc)
        | Min (_, y, z) -> intpred y (intvalue z acc)
    and intarray ast acc =
        match ast with
        | IntArrayConst (ConstName name) -> (name, IntArrayType) :: acc
    and intset ast acc =
        match ast with
        | Range (l, r) -> intvalue l (intvalue r acc)
    and intpred ast acc =
        match ast with
        | In x -> intset x acc
        | Lt x -> intvalue x acc
    in Map (intvalue ast [])

let toCXXType type_ =
    match type_ with
    | IntType -> "int64_t"
    | IntArrayType -> "const vector<int64_t> &"


let format ast =
    let sb = new StringBuilder()
    let _ = sb.Append "int64_t solve("
    let _ = freevars ast |> Map.toList |> List.map (fun (key, value) -> sprintf "%s %s" (toCXXType value) key) |> String.concat ", " |> sb.Append
    let _ = sb.Append ") {\n"

    let mutable counter = 0
    let newvar () =
        counter <- counter + 1
        sprintf "t%d" (counter - 1)
    let mutable nest = 1
    let indent () = String.replicate nest "    "
    let rec go ast =
        let binop c x y =
            match (go x, go y) with
            | (Some x, Some y) -> Some (sprintf "(%s %c %s)" x c y)
            | _ -> None

        let bigop unit_ append x y z =
            match x with
            | VarName x ->
                let name = newvar ()
                let _ = sb.Append (sprintf "%sint64_t %s = %s;\n" (indent ()) name unit_)
                let loop forstmt =
                    let _ = sb.Append (sprintf "%s%s {\n" (indent ()) forstmt)
                    nest <- nest + 1
                    match go z with
                    | None -> None
                    | Some s ->
                        let _ = sb.Append (sprintf "%s%s;\n" (indent ()) (append name s))
                        nest <- nest - 1
                        let _ = sb.Append (sprintf "%s}\n" (indent ()))
                        Some name

                match y with
                | Lt n ->
                    match go n with
                    | None -> None
                    | Some n ->
                        loop (sprintf "for (int64_t %s = 0; %s < %s; ++ %s)"  x x n x)
                | In (Range (l, r)) ->
                    match (go l, go r) with
                    | (Some l, Some r) ->
                        loop (sprintf "for (int64_t %s = %s; %s < %s; ++ %s)"  x l x r x)
                    | _ -> None

        match ast with
        | Int value -> Some (string value)
        | IntVar (VarName name) -> Some name
        | IntConst (ConstName name) -> Some name
        | Subscript (IntArrayConst (ConstName ary), y) ->
            match go y with
            | Some ix -> Some (sprintf "%s[%s]" ary ix)
            | None -> None
        | Add (x, y) -> binop '+' x y
        | Sub (x, y) -> binop '-' x y
        | Mul (x, y) -> binop '*' x y
        | Div (x, y) -> binop '/' x y
        | Mod (x, y) -> binop '%' x y

        | Sum (x, y, z) -> bigop "0" (sprintf "%s += %s") x y z
        | Max (x, y, z) -> bigop "INT64_MIN" (fun a b -> sprintf "%s = max(%s, %s)" a a b) x y z
        | Min (x, y, z) -> bigop "INT64_MAX" (fun a b -> sprintf "%s = min(%s, %s)" a a b) x y z

    match go ast with
    | None -> None
    | Some name ->
        let _ = sb.Append (sprintf "%sreturn %s;\n" (indent ()) name)
        let _ = sb.Append "}\n"
        Some (sb.ToString ())


[<EntryPoint>]
let main argv =
    printf "Input: "
    let s = Console.ReadLine ()
    // let s = "\\sum _ {i < N} A _ i"

    match run parser s with
    | Success (result, state, position) ->
        match format result with
        | Some result -> printfn "Success:\n%s" result
        | None -> printfn "Failure:"
    | Failure (message, error, state) -> printfn "Failure: %s" message
    0 // return an integer exit code
