module FiniteStateMachine

open Tokens
open System

let createToken (input: TokenizerInput) (tokenType: TokenType) (lexeme: string) =
    let line = match (tokenType, lexeme) with
                    | NEWLINE, _ -> input.Line + 1
                    | _ -> input.Line
    let length = match (tokenType, lexeme) with
                    | STRING, _ -> lexeme.Length + 2
                    | _, "" -> 1
                    | _ -> lexeme.Length
    let column = if tokenType = STRING then input.Column + length + 2 else input.Column + length

    ({ TokenType = tokenType; Lexeme = lexeme; Line = line; Column = column }, { Input = input.Input.Substring(length); Line = line; Column = column })
    
let nextState (curr: TokenizerInput) (accumulator: Token list) (transition: TokenizerInput -> Token * TokenizerInput) internalFn =
    let nextToken, curr = transition curr
    let nextAccumulator = match nextToken.TokenType with
                            | WHITESPACE -> accumulator
                            | NEWLINE -> accumulator
                            | _ -> accumulator @ [ nextToken ]
    internalFn curr nextAccumulator

let whitespaceState (curr: TokenizerInput) =
    let whitespace, curr = createToken curr WHITESPACE " "
    (whitespace, curr)
    
let newlineState (curr: TokenizerInput) =
    let newline, curr = createToken curr NEWLINE "\n"
    (newline, curr)

let letterState (reservedMap: Map<string, TokenType>) (curr: TokenizerInput) =
    let string = curr.Input |> Seq.takeWhile Char.IsLetter |> Seq.toList |> String.Concat

    let tokenType =
        match reservedMap |> Map.tryFind string with
        | Some tokenType -> tokenType
        | None -> IDENTIFIER

    let identifier, curr = createToken curr tokenType string
    (identifier, curr)
    
let numberState (curr: TokenizerInput) =
    let string = curr.Input |> Seq.takeWhile Char.IsNumber |> Seq.toList |> String.Concat

    let number, curr = createToken curr NUMBER string
    (number, curr)
    
let symbolState (curr: TokenizerInput) =
    let string = curr.Input |> Seq.takeWhile (fun c -> Char.IsSymbol(c) || Char.IsPunctuation(c)) |> Seq.toList |> String.Concat

    let tokenType =
        match reservedKeywordsAndSymbolsMap |> Map.tryFind string with
        | Some(tokenType) -> tokenType
        | None -> IDENTIFIER

    let identifier, curr = createToken curr tokenType string
    (identifier, curr)
    
let stringState (curr: TokenizerInput) =
    let chars = curr.Input |> Seq.skip 1 |> Seq.takeWhile (fun c -> c <> '"') |> Seq.toList

    let string, curr = createToken curr STRING (String.Concat(chars))
    (string, curr)
    
let defaultState (curr: TokenizerInput) =
    let error, curr = createToken curr IDENTIFIER (curr.Input.[0].ToString())
    (error, curr)

let fsmTokenizer (curr: TokenizerInput) (accumulator: Token list) =
    let map = reservedKeywordsAndSymbolsMap

    let rec internalFn (curr: TokenizerInput) (accumulator: Token list) =
        if curr.Input.Length = 0 then
            accumulator @ [ { Lexeme = ""; TokenType = EOF; Line = curr.Line; Column = curr.Column } ]
        else
            match curr.Input[0] with
            | char when char = '"' -> nextState curr accumulator stringState internalFn
            | char when char = '\n' -> nextState curr accumulator newlineState internalFn
            | char when Char.IsWhiteSpace(char) -> nextState curr accumulator whitespaceState internalFn
            | char when (Char.IsSymbol(char) || Char.IsPunctuation(char)) -> nextState curr accumulator symbolState internalFn
            | char when Char.IsLetter(char) -> nextState curr accumulator (letterState map) internalFn
            | char when Char.IsNumber(char) -> nextState curr accumulator numberState internalFn
            | _ -> nextState curr accumulator defaultState internalFn

    internalFn curr accumulator
