module Parsers

open ParserCombinator
open Tokens

let parseIdentifierContent = choice [ fromAnyOf [ 'a' .. 'z' ]; fromAnyOf [ 'A' .. 'Z' ] ] |> many1 <!> (fun x -> x |> List.map (fun c -> c.ToString()) |> String.concat "")

let parseIdentifier = parseIdentifierContent <!> (fun x -> { Lexeme = x; TokenType = IDENTIFIER })

let parseStringContent =
    choice [ fromAnyOf [ 'a' .. 'z' ]; fromAnyOf [ 'A' .. 'Z' ]; fromAnyOf [ ' '; '\t'; '\n'; '\r' ] ] |> many1 <!> (fun x -> x |> List.map (fun c -> c.ToString()) |> String.concat "")

let parseString =
    let firstQuoteParser = fromString "\""
    let secondQuoteParser = fromString "\""
    let stringContentParser = parseStringContent

    between firstQuoteParser stringContentParser secondQuoteParser <!> (fun x -> { Lexeme = x; TokenType = STRING })

let parseNewLine = fromChar '\n' <!> (fun x -> { Lexeme = x.ToString(); TokenType = NEWLINE })

let parseWhitespace = fromChar ' ' |> many1 <!> (fun x -> { Lexeme = x |> List.map (fun c -> c.ToString()) |> String.concat ""; TokenType = WHITESPACE })

let parseNumber = choice [ fromAnyOf [ '0' .. '9' ] ] |> many1 <!> (fun x -> { Lexeme = x |> List.map (fun c -> c.ToString()) |> String.concat ""; TokenType = NUMBER })

let createSymbolParser (input: string, tokenType: TokenType) = fromString input <!> (fun x -> { Lexeme = x; TokenType = tokenType })

// ----------- Implementation -----------
let parsers =
    choice ([ parseIdentifier; parseString; parseWhitespace; parseNewLine; parseNumber ] |> List.append (reservedKeywordsAndSymbols |> List.map (fun (input, tokenType) -> createSymbolParser (input, tokenType))))

let rec tokenizeUsingParsers (curr: TokenizerInput) (accumulator: Token list) =
    match curr.Input.Trim() with
    | "" -> accumulator @ [ { Lexeme = ""; TokenType = EOF; Line = curr.Line; Column = curr.Column + 1 } ]
    | _ ->
        let result = run parsers curr

        match result with
        | Success(output) ->
            let token = { TokenType = output.value.TokenType; Lexeme = output.value.Lexeme; Line = output.remainingInput.Line; Column = output.remainingInput.Column }

            let newAccumulator = if token.TokenType <> WHITESPACE then accumulator @ [ token ] else accumulator

            tokenizeUsingParsers output.remainingInput newAccumulator
        | Failure(_) -> accumulator
