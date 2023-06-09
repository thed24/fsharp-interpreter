module StatementEvaluationTests

open System
open System.IO
open Evaluation
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
    let context = evaluateStatements statements { Variables = Map.empty; Errors = []; Enclosing = None } 
    
    // assert'
    match context.Errors with
    | [] -> ()
    | _ -> failwith "Expected no errors"
    
    match context.Variables.["a"] with
    | PrimaryValue.Number actual -> Assert.Equal(expected, actual)
    | _ -> failwith "Expected number"
    
[<Fact>]
let ``Given multiple lexical scopes, handles variables at each scope correctly`` () =
    // i know this is bad practice, but it's a fun lil side project, don't judge me
    
    // arrange
    let stringWriter = new StringWriter()
    Console.SetOut(stringWriter)
    
    let input = @"
var a = ""global a"";
var b = ""global b"";
var c = ""global c"";
{
  var a = ""outer a"";
  var b = ""outer b"";
  {
    var a = ""inner a"";
    print a;
    print b;
    print c;
  }
  print a;
  print b;
  print c;
}
print a;
print b;
print c;
"

    // act
    let tokens = tokenize fsmTokenizer input
    let statements, remaining = statement { Tokens = tokens; Errors = []; } []
    let context = evaluateStatements statements { Variables = Map.empty; Errors = []; Enclosing = None }
    
    // assert
    match context.Errors with
    | [] -> ()
    | _ -> failwith "Expected no errors"
    
    let actual = stringWriter.ToString()
    let expected = "inner a\r\nouter b\r\nglobal c\r\nouter a\r\nouter b\r\nglobal c\r\nglobal a\r\nglobal b\r\nglobal c\r\n"

    Assert.Equal(expected, actual)

[<Fact>]
let ``Given an if statement, evaluates the correct branch`` () =
    // again with the bad practice, but it's a fun lil side project, don't judge me
    
    // arrange
    let stringWriter = new StringWriter()
    Console.SetOut(stringWriter)
    
    let input = @"
var a = 1;
if (a == 1) {
  print ""a is 1"";
} else {
  print ""a is not 1"";
}
"

    // act
    let tokens = tokenize fsmTokenizer input
    let statements, remaining = statement { Tokens = tokens; Errors = []; } []
    let context = evaluateStatements statements { Variables = Map.empty; Errors = []; Enclosing = None }
    
    // assert
    match context.Errors with
    | [] -> ()
    | _ -> failwith "Expected no errors"
    
    let actual = stringWriter.ToString()
    let expected = "a is 1\r\n"

    Assert.Equal(expected, actual)

[<Fact>]
let ``Given multiple logical operators, evaluates correctly`` () =
    // arrange
    let input = "var a = 1 == 1 and 2 == 2 and 3 == 3;"
    let expected = true
    
    // act
    let tokens = tokenize fsmTokenizer input
    let statements, remaining = statement { Tokens = tokens; Errors = []; } []
    let context = evaluateStatements statements { Variables = Map.empty; Errors = []; Enclosing = None } 
    
    // assert'
    match context.Errors with
    | [] -> ()
    | _ -> failwith "Expected no errors"
    
    match context.Variables.["a"] with
    | PrimaryValue.Boolean actual -> Assert.Equal(expected, actual)
    | _ -> failwith "Expected boolean"

[<Fact>]
let ``Given a true expression, while loop executes`` () =
    // arrange
    let stringWriter = new StringWriter()
    Console.SetOut(stringWriter)
    
    let input = @"
var a = 1;
while (a < 10) {
  print a;
  a = a + 1;
}

print a;
"

    // act
    let tokens = tokenize fsmTokenizer input
    let statements, remaining = statement { Tokens = tokens; Errors = []; } []
    let context = evaluateStatements statements { Variables = Map.empty; Errors = []; Enclosing = None }
    
    // assert
    match context.Errors with
    | [] -> ()
    | _ -> failwith "Expected no errors"
    
    let actual = stringWriter.ToString()
    let expected = "1.000000\r\n2.000000\r\n3.000000\r\n4.000000\r\n5.000000\r\n6.000000\r\n7.000000\r\n8.000000\r\n9.000000\r\n10.000000\r\n"

    Assert.Equal(expected, actual)
    
    match context.Variables.["a"] with
    | PrimaryValue.Number actual -> Assert.Equal(10.0, actual)
    | _ -> failwith "Expected number"

[<Fact>]
let ``Given a false expression, while loop does not execute`` () =
    // arrange
    let stringWriter = new StringWriter()
    Console.SetOut(stringWriter)
    
    let input = @"
var a = 10;
while (a < 10) {
  print a;
  a = a + 1;
}

print a;
"

    // act
    let tokens = tokenize fsmTokenizer input
    let statements, remaining = statement { Tokens = tokens; Errors = []; } []
    let context = evaluateStatements statements { Variables = Map.empty; Errors = []; Enclosing = None }
    
    // assert
    match context.Errors with
    | [] -> ()
    | _ -> failwith "Expected no errors"
    
    let actual = stringWriter.ToString()
    let expected = "10.000000\r\n"

    Assert.Equal(expected, actual)
    
    match context.Variables.["a"] with
    | PrimaryValue.Number actual -> Assert.Equal(10.0, actual)
    | _ -> failwith "Expected number"

[<Fact>]
let ``Given a true expression, for loop executes`` () =
    // arrange
    let stringWriter = new StringWriter()
    Console.SetOut(stringWriter)
    
    let input = @"
for (var a = 1; a < 10; a = a + 1) {
    print a;
}
"

    // act
    let tokens = tokenize fsmTokenizer input
    let statements, remaining = statement { Tokens = tokens; Errors = []; } []
    let context = evaluateStatements statements { Variables = Map.empty; Errors = []; Enclosing = None }
    
    // assert
    match context.Errors with
    | [] -> ()
    | _ -> failwith "Expected no errors"
    
    let actual = stringWriter.ToString()
    let expected = "1.000000\r\n2.000000\r\n3.000000\r\n4.000000\r\n5.000000\r\n6.000000\r\n7.000000\r\n8.000000\r\n9.000000\r\n"

    Assert.Equal(expected, actual)
    