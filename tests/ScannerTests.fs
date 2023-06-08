module Tests

open Scanner
open Xunit

[<Fact>]
let ``Given a string that matches parser, successfully match`` () =
    // arrange
    let parseVar = Parser.fromString "var"
    let input = "var"

    // act
    let result = parseVar.run input

    // assert
    match result with
    | Success(value, remaining) -> Assert.Equal("var", value)
    | Failure err -> Assert.True(false)

[<Fact>]
let ``Given a string that does not match parser, fail`` () =
    // arrange
    let parseVar = Parser.fromString "var"
    let input = "let"

    // act
    let result = parseVar.run input

    // assert
    match result with
    | Success(value, remaining) -> Assert.True(false)
    | Failure err -> Assert.Equal("Error", err)

[<Fact>]
let ``Given a list of chars, successfully match a char and return the remaining string`` () =
    // arrange
    let parseVar = Parser.fromAnyOf ['v'; 'a'; 'r']
    let input = "rock"

    // act
    let result = parseVar.run input

    // assert
    match result with
    | Success(value, remaining) -> Assert.Equal("ock", remaining)
    | Failure err -> Assert.True(false)

[<Fact>]
let ``Given a list of chars, fail to match a char and return the remaining string`` () =
    // arrange
    let parseVar = Parser.fromAnyOf ['v'; 'a'; 'r']
    let input = "let"

    // act
    let result = parseVar.run input

    // assert
    match result with
    | Success(value, remaining) -> Assert.True(false)
    | Failure err -> Assert.True(true)
