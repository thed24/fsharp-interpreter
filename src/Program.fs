module Program

open Scanner
open Input
open Tokens
open System

// ---------- Parsers ------------
let parseIdentifierContent =
    Parser.choice [ Parser.fromAnyOf<char list> ['a'..'z']; Parser.fromAnyOf<char list> ['A'..'Z'] ]
    |> (fun parser -> parser.many1)
    <!> (fun x -> x |> List.map (fun c -> c.ToString()) |> String.concat "")

let parseIdentifier =  
    parseIdentifierContent
    <!> (fun x -> { lexeme = x; tokenType = IDENTIFIER; })

let parseStringContent =
    Parser.choice [ Parser.fromAnyOf<char list> ['a'..'z']; Parser.fromAnyOf<char list> ['A'..'Z']; Parser.fromAnyOf<char list> [' '; '\t'; '\n'; '\r'] ]
    |> (fun parser -> parser.many1)
    <!> (fun x -> x |> List.map (fun c -> c.ToString()) |> String.concat "")

let parseString = 
    let firstQuoteParser = Parser.fromString<String> "\""
    let secondQuoteParser = Parser.fromString<String> "\""

    parseStringContent.between firstQuoteParser secondQuoteParser
    <!> (fun x -> { lexeme = x; tokenType = STRING; })

let parseNewLine = 
    Parser.fromChar<char> ('\n', true)
    <!> (fun x -> { lexeme = x.ToString(); tokenType = NEWLINE; })

let parseWhitespace = 
    Parser.fromChar<char> (' ', false)
    |> (fun parser -> parser.many1)
    <!> (fun x -> { lexeme = x |> List.map (fun c -> c.ToString()) |> String.concat ""; tokenType = WHITESPACE; })

let createSymbolParser (input: string, tokenType: TokenType) =
    Parser.fromString<String> input
    <!> (fun x -> { lexeme = x; tokenType = tokenType; })

let parsers = Parser.choice (
    [ parseIdentifier; parseString; parseWhitespace; parseNewLine ]
    |> List.append (reservedKeywordsAndSymbols |> List.map (fun (input, tokenType) -> createSymbolParser (input, tokenType)))
)

// ---------- Lexing ------------
let rec parse (curr: ParserInput) (accumulator: Token list) =
    match curr.input.Trim() with
    | "" -> accumulator @ [{ lexeme = ""; tokenType = EOF; line = curr.line; column = curr.column + 1; }]
    | _ -> 
        let result = parsers.run curr

        match result with
        | Success (output) ->
            let token = { tokenType = output.value.tokenType; lexeme = output.value.lexeme; line = output.remainingInput.line; column = output.remainingInput.column; }
            let newAccumulator = accumulator @ [token]
            parse output.remainingInput newAccumulator
        | Failure (message) ->
            printfn "Error: %s" message
            accumulator

[<EntryPoint>]
let main args =
    let input = readFileFromPath "../assets/input.txt" |> joinLines

    let tokens = parse { input = input; column = 0; line = 1; } []

    tokens |> List.iter (fun token -> printfn "%A" token)

    0
