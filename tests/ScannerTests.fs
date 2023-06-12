module Tests

open Scanner
open Xunit
open System

[<Fact>]
let ``Given a string that matches parser, successfully match`` () =
    // arrange
    let parseVar = Parser.fromString<char list> "var"
    let input = { input = "var"; line = 0; column = 0}

    // act
    let result = parseVar.run input

    // assert
    match result with
    | Success(result) -> Assert.Equal({ value = "var"; remainingInput = { input = ""; line = 0; column = 3} }, result)
    | Failure err -> Assert.True(false)

[<Fact>]
let ``Given a string that does not match parser, fail`` () =
    // arrange
    let parseVar = Parser.fromString<char list> "var"
    let input = { input = "let"; line = 0; column = 0}

    // act
    let result = parseVar.run input

    // assert
    match result with
    | Success(value) -> Assert.True(false)
    | Failure err -> Assert.True(true)

[<Fact>]
let ``Given a list of chars, successfully match a char and return the remaining string`` () =
    // arrange
    let parseVar = Parser.fromAnyOf<char list> ['v'; 'a'; 'r']
    let input = { input = "rock"; line = 0; column = 0}

    // act
    let result = parseVar.run input

    // assert
    match result with
    | Success(result) -> Assert.Equal({ value = 'r'; remainingInput = { input = "ock"; line = 0; column = 1} }, result)
    | Failure err -> Assert.True(false)

[<Fact>]
let ``Given a list of chars, fail to match a char and return the remaining string`` () =
    // arrange
    let parseVar = Parser.fromAnyOf<char list> ['v'; 'a'; 'r']
    let input = { input = "let"; line = 0; column = 0}

    // act
    let result = parseVar.run input

    // assert
    match result with
    | Success(result) -> Assert.True(false)
    | Failure err -> Assert.True(true)

[<Fact>]
let ``Given a char parser and then another char parser, successfully match both`` () =
    // arrange
    let firstParser = Parser.fromAnyOf<char list> ['v';]
    let secondParser = Parser.fromAnyOf<char list> ['a'; 'r']
    let parseVarLet = firstParser.andThen secondParser
    let input = { input = "var"; line = 0; column = 0}

    // act
    let result = parseVarLet.run input

    // assert
    match result with
    | Success(result) -> Assert.Equal({ value = ('v', 'a'); remainingInput = { input = "r"; line = 0; column = 2} }, result)
    | Failure err -> Assert.True(false)

[<Fact>]
let ``Given a char parser and then another char parser, fail to match both`` () =
    // arrange
    let firstParser = Parser.fromAnyOf<char list> ['v';]
    let secondParser = Parser.fromAnyOf<char list> ['a'; 'r']
    let parseVarLet = firstParser.andThen secondParser
    let input = { input = "let"; line = 0; column = 0}

    // act
    let result = parseVarLet.run input

    // assert
    match result with
    | Success(result) -> Assert.True(false)
    | Failure err -> Assert.True(true)

[<Fact>]
let ``Given a between parser for a string, when parsing a string between quotes, successfully match`` () =
    // arrange
    let parseVar = Parser.fromString<String> "var"
    let parseQuote = Parser.fromString<String> "\""
    let parseVarBetweenQuotes = parseVar.between parseQuote parseQuote
    let input = { input = "\"var\""; line = 0; column = 0}

    // act
    let result = parseVarBetweenQuotes.run input

    // assert
    match result with
    | Success(result) -> Assert.Equal({ value = "var"; remainingInput = { input = ""; line = 0; column = 5} }, result)
    | Failure err -> Assert.True(false)

[<Fact>]
let ``Given a between parser for a string, when parsing a different string between quotes, fail to match`` () =
    // arrange
    let parseVar = Parser.fromString<String> "var"
    let parseQuote = Parser.fromString<String> "\""
    let parseVarBetweenQuotes = parseVar.between parseQuote parseQuote
    let input = { input = "\"let\""; line = 0; column = 0}

    // act
    let result = parseVarBetweenQuotes.run input

    // assert
    match result with
    | Success(result) -> Assert.True(false)
    | Failure err -> Assert.True(true)
