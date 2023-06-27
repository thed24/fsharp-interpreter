module StatementEvaluationTests

open Expression
open StatementEvaluation
open Statement
open Tokenizer
open FiniteStateMachine
open Xunit

[<Fact>]
let ``Given addition and multiplication, returns result with precedence honored`` () =
    // arrange
    let input = "var a = 1 + 2 * 3;"
    let expected = 7.0
    
    // act
    let tokens = tokenize fsmTokenizer input
    let statements, remaining = statement { Tokens = tokens; Errors = []; } []
    let context = evaluateStatements (List.rev statements) { Variables = Map.empty; Errors = [] }
    
    // assert'
    match context.Errors with
    | [] -> ()
    | _ -> failwith "Expected no errors"
    
    match context.Variables.["a"] with
    | PrimaryValue.Number actual -> Assert.Equal(expected, actual)
    | _ -> failwith "Expected number"