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
    let input = System.IO.File.ReadAllText("input.txt")
    
    let tokens = tokenize fsmTokenizer input

    // let syntax, state = expression tokens
    let statements, state = statement { Tokens = tokens; Errors = []; } []
    
    // let result = evaluateExpression syntax
    let maybeErrors = evaluateStatements (List.rev statements) { Variables = Map.empty; Errors = []; Enclosing = None; }

    0
