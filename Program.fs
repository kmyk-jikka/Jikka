open System
open FSharp.Text.Lexing

[<EntryPoint>]
let main argv =
    let lexbuf = LexBuffer<char>.FromTextReader Console.In
    let parsed =
        try
            Parser.start Lexer.tokenstream lexbuf
        with
            | e when e.Message.Equals "parse error" ->
                raise (Exception (sprintf "SyntaxError: Unexpected token: \"%s\" Line: %d Column: %d" (LexBuffer<_>.LexemeString lexbuf) (lexbuf.StartPos.Line + 1) lexbuf.StartPos.Column))
    printfn "%A" parsed
    0
