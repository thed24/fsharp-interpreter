module Program

open Tokenizer
open Expression
open FiniteStateMachine
open ExpressionEvaluation
open StatementEvaluation
open Statement

[<EntryPoint>]
let main _ =
    let input = System.IO.File.ReadAllText("input.txt")
    
    let tokens = tokenize fsmTokenizer input

    let statements, state = statement { Tokens = tokens; Errors = []; } []
    
    let maybeErrors = evaluateStatements statements { Variables = Map.empty; Errors = []; Enclosing = None; }

    0
