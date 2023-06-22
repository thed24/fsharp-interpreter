module Program

open Tokenizer
open Expression
open FiniteStateMachine
open ExpressionEvaluation
open StatementEvaluation
open Statement

[<EntryPoint>]
let main _ =
    //let input = "1 + 2 * \"test\""
    let input = "print 1 + 2 * 3"
    
    let tokens = tokenize tokenizeUsingFsm input

    // let syntax, state = expression tokens
    let syntax, state = statement tokens
    
    // let result = evaluateExpression syntax
    let maybeError = evaluateStatement syntax

    // printfn "Result: %A" result

    match maybeError with
    | Some error -> printfn "Error: %s" error
    | None -> printfn "Success"

    0
