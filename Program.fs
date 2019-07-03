open System
open FSharp.Text.Lexing

[<EntryPoint>]
let main argv =
    let text = "given N : Int compute 3" 
    let lexbuf = LexBuffer<char>.FromString text
    // let lexbuf = LexBuffer<char>.FromTextReader Console.In
    let parsed = Parser.start Lexer.tokenstream lexbuf
    printfn "%A" parsed
    0
