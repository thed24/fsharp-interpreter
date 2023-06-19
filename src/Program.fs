module Program

open Tokenizer
open Syntax
open FiniteStateMachine

[<EntryPoint>]
let main _ =
    let input = "4 + 2 - 1"

    let tokens = tokenize tokenizeUsingFsm input
    let syntax, state = expression tokens

    printfn "Tokens:"
    tokens |> List.iter (fun token -> printfn $"%A{token}")

    printfn "Syntax:"
    printfn $"%A{prettyPrint syntax}"

    printfn "Syntax Errors:"
    printfn $"%A{prettyPrintErrors state.Errors}"

    0
