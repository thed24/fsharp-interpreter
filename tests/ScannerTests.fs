module Tests

open Scanner
open Xunit
open System

[<Fact>]
let ``Given a string that matches parser, successfully match`` () =
    // arrange
    let parseVar = fromString "var"
    let input = { input = "var"; line = 0; column = 0}

    // act
    let result = run parseVar input

    // assert
    match result with
    | Success(result) -> Assert.Equal({ value = "var"; remainingInput = { input = ""; line = 0; column = 3} }, result)
    | Failure err -> Assert.True(false)

[<Fact>]
let ``Given a string that does not match parser, fail`` () =
    // arrange
    let parseVar = fromString "var"
    let input = { input = "let"; line = 0; column = 0}

    // act
    let result = run parseVar input

    // assert
    match result with
    | Success(value) -> Assert.True(false)
    | Failure err -> Assert.True(true)

[<Fact>]
let ``Given a list of chars, successfully match a char and return the remaining string`` () =
    // arrange
    let parseVar = fromAnyOf ['v'; 'a'; 'r']
    let input = { input = "rock"; line = 0; column = 0}

    // act
    let result = run parseVar input

    // assert
    match result with
    | Success(result) -> Assert.Equal({ value = 'r'; remainingInput = { input = "ock"; line = 0; column = 1} }, result)
    | Failure err -> Assert.True(false)

[<Fact>]
let ``Given a list of chars, fail to match a char and return the remaining string`` () =
    // arrange
    let parseVar = fromAnyOf ['v'; 'a'; 'r']
    let input = { input = "let"; line = 0; column = 0}

    // act
    let result = run parseVar input

    // assert
    match result with
    | Success(result) -> Assert.True(false)
    | Failure err -> Assert.True(true)

[<Fact>]
let ``Given a char parser and then another char parser, successfully match both`` () =
    // arrange
    let firstParser = fromAnyOf ['v';]
    let secondParser = fromAnyOf ['a'; 'r']
    let parseVarLet = andThen firstParser secondParser
    let input = { input = "var"; line = 0; column = 0}

    // act
    let result = run parseVarLet input

    // assert
    match result with
    | Success(result) -> Assert.Equal({ value = ('v', 'a'); remainingInput = { input = "r"; line = 0; column = 2} }, result)
    | Failure err -> Assert.True(false)

[<Fact>]
let ``Given a char parser and then another char parser, fail to match both`` () =
    // arrange
    let firstParser = fromAnyOf ['v';]
    let secondParser = fromAnyOf ['a'; 'r']
    let parseVarLet = andThen firstParser secondParser
    let input = { input = "let"; line = 0; column = 0}

    // act
    let result = run parseVarLet input

    // assert
    match result with
    | Success(result) -> Assert.True(false)
    | Failure err -> Assert.True(true)

[<Fact>]
let ``Given a between parser for a string, when parsing a string between quotes, successfully match`` () =
    // arrange
    let parseVar = fromString "var"
    let parseQuote = fromString "\""
    let parseVarBetweenQuotes = between parseQuote parseVar parseQuote
    let input = { input = "\"var\""; line = 0; column = 0}

    // act
    let result = run parseVarBetweenQuotes input

    // assert
    match result with
    | Success(result) -> Assert.Equal({ value = "var"; remainingInput = { input = ""; line = 0; column = 5} }, result)
    | Failure err -> Assert.True(false)

[<Fact>]
let ``Given a between parser for a string, when parsing a different string between quotes, fail to match`` () =
    // arrange
    let parseVar = fromString "var"
    let parseQuote = fromString "\""
    let parseVarBetweenQuotes = between parseQuote parseVar parseQuote
    let input = { input = "\"let\""; line = 0; column = 0}

    // act
    let result = run parseVarBetweenQuotes input

    // assert
    match result with
    | Success(result) -> Assert.True(false)
    | Failure err -> Assert.True(true)
