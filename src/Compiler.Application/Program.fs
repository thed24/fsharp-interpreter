module Program

open Tokenizer
open Syntax
open FiniteStateMachine
open Evaluating

[<EntryPoint>]
let main _ =
    let input = "1 + 2 * 7.32"
    let tokens = tokenize tokenizeUsingFsm input
    let syntax, state = expression tokens
    let result = evaluate syntax

    printfn "Tokens:"
    tokens |> List.iter (fun token -> printfn $"%A{token}")

    printfn "Syntax:"
    printfn $"%A{prettyPrint syntax}"

    printfn "Syntax Errors:"
    printfn $"%A{prettyPrintErrors state.Errors}"
    
    printfn "Evaluation:"
    printfn $"%A{result}"

    0
