module PcTests

open ParserCombinator
open Xunit
open Tokens

[<Fact>]
let ``Given a string that matches parser, successfully match`` () =
    // arrange
    let parseVar = fromString "var"
    let input = { Input = "var"; Line = 0; Column = 0 }

    // act
    let result = run parseVar input

    // assert
    match result with
    | Success(result) -> Assert.Equal({ value = "var"; remainingInput = { Input = ""; Line = 0; Column = 3 } }, result)
    | Failure _ -> Assert.True(false)

[<Fact>]
let ``Given a string that does not match parser, fail`` () =
    // arrange
    let parseVar = fromString "var"
    let input = { Input = "let"; Line = 0; Column = 0 }

    // act
    let result = run parseVar input

    // assert
    match result with
    | Success _ -> Assert.True(false)
    | Failure _ -> Assert.True(true)

[<Fact>]
let ``Given a list of chars, successfully match a char and return the remaining string`` () =
    // arrange
    let parseVar = fromAnyOf [ 'v'; 'a'; 'r' ]
    let input = { Input = "rock"; Line = 0; Column = 0 }

    // act
    let result = run parseVar input

    // assert
    match result with
    | Success(result) -> Assert.Equal({ value = 'r'; remainingInput = { Input = "ock"; Line = 0; Column = 1 } }, result)
    | Failure _ -> Assert.True(false)

[<Fact>]
let ``Given a list of chars, fail to match a char and return the remaining string`` () =
    // arrange
    let parseVar = fromAnyOf [ 'v'; 'a'; 'r' ]
    let input = { Input = "let"; Line = 0; Column = 0 }

    // act
    let result = run parseVar input

    // assert
    match result with
    | Success _ -> Assert.True(false)
    | Failure _ -> Assert.True(true)

[<Fact>]
let ``Given a char parser and then another char parser, successfully match both`` () =
    // arrange
    let firstParser = fromAnyOf [ 'v' ]
    let secondParser = fromAnyOf [ 'a'; 'r' ]
    let parseVarLet = andThen firstParser secondParser
    let input = { Input = "var"; Line = 0; Column = 0 }

    // act
    let result = run parseVarLet input

    // assert
    match result with
    | Success(result) -> Assert.Equal({ value = ('v', 'a'); remainingInput = { Input = "r"; Line = 0; Column = 2 } }, result)
    | Failure _ -> Assert.True(false)

[<Fact>]
let ``Given a char parser and then another char parser, fail to match both`` () =
    // arrange
    let firstParser = fromAnyOf [ 'v' ]
    let secondParser = fromAnyOf [ 'a'; 'r' ]
    let parseVarLet = andThen firstParser secondParser
    let input = { Input = "let"; Line = 0; Column = 0 }

    // act
    let result = run parseVarLet input

    // assert
    match result with
    | Success _ -> Assert.True(false)
    | Failure _ -> Assert.True(true)

[<Fact>]
let ``Given a between parser for a string, when parsing a string between quotes, successfully match`` () =
    // arrange
    let parseVar = fromString "var"
    let parseQuote = fromString "\""
    let parseVarBetweenQuotes = between parseQuote parseVar parseQuote

    let input = { Input = "\"var\""; Line = 0; Column = 0 }

    // act
    let result = run parseVarBetweenQuotes input

    // assert
    match result with
    | Success(result) -> Assert.Equal({ value = "var"; remainingInput = { Input = ""; Line = 0; Column = 5 } }, result)
    | Failure _ -> Assert.True(false)

[<Fact>]
let ``Given a between parser for a string, when parsing a different string between quotes, fail to match`` () =
    // arrange
    let parseVar = fromString "var"
    let parseQuote = fromString "\""
    let parseVarBetweenQuotes = between parseQuote parseVar parseQuote

    let input = { Input = "\"let\""; Line = 0; Column = 0 }

    // act
    let result = run parseVarBetweenQuotes input

    // assert
    match result with
    | Success _ -> Assert.True(false)
    | Failure _ -> Assert.True(true)
