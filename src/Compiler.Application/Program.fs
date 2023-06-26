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
    let input = "var a = 1 + 2 * 3; print a; var a = \"test\"; print a;"
    
    let tokens = tokenize tokenizeUsingFsm input

    // let syntax, state = expression tokens
    let statements, state = statement { Tokens = tokens; Errors = []; } []
    
    // let result = evaluateExpression syntax
    let maybeErrors = evaluateStatements (List.rev statements) { Variables = Map.empty; Errors = [] }

    0
