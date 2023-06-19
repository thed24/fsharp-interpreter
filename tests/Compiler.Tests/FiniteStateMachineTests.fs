module FsmTests

open FiniteStateMachine
open Xunit
open Tokens

[<Fact>]
let ``Given an identifier, successfully match`` () =
    // arrange
    let input = { Input = "var"; Line = 0; Column = 0 }

    // act
    let result = tokenizeUsingFsm input []

    // assert
    let expected: Token list = [ { TokenType = VAR; Lexeme = "var"; Line = 0; Column = 3 }; { TokenType = EOF; Lexeme = ""; Line = 0; Column = 3 } ]

    let actual = result

    Assert.Equivalent(expected, actual)

[<Fact>]
let ``Given a simple equation, successfully match`` () =
    // arrange
    let input = { Input = "var x = 1 + 2"; Line = 0; Column = 0 }

    // act
    let result = tokenizeUsingFsm input []

    // assert
    let expected: Token list =
        [ { TokenType = VAR; Lexeme = "var"; Line = 0; Column = 3 }
          { TokenType = IDENTIFIER; Lexeme = "x"; Line = 0; Column = 5 }
          { TokenType = EQUAL; Lexeme = "="; Line = 0; Column = 7 }
          { TokenType = NUMBER; Lexeme = "1"; Line = 0; Column = 9 }
          { TokenType = PLUS; Lexeme = "+"; Line = 0; Column = 11 }
          { TokenType = NUMBER; Lexeme = "2"; Line = 0; Column = 13 }
          { TokenType = EOF; Lexeme = ""; Line = 0; Column = 13 } ]

    let actual = result

    Assert.Equivalent(expected, actual)
