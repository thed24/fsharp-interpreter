module ExpressionEvaluationTests

open ExpressionEvaluation
open Expression
open Tokenizer
open FiniteStateMachine
open Xunit

[<Fact>]
let ``Given addition and multiplication, returns result with precedence honored`` () =
    // arrange
    let input = "1 + 2 * 3"
    let expected = 7.0
    
    // act
    let tokens = tokenize fsmTokenizer input
    let expression, remaining = expression tokens
    let result = evaluateExpression expression
    
    // assert'
    match result with
    | ExpressionResult.Success value -> match value with
                                        | PrimaryValue.Number value -> Assert.Equal(expected, value)
                                        | _ -> Assert.True(false, "Expected an expression") 
    | _ -> Assert.True(false, "Expected an integer value")