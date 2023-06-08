module Scanner

open Tokens

let charListToStr charList =
    charList |> List.toArray |> System.String

type ParserResult<'a> =
    | Success of 'a
    | Failure of string with 
    member this.map (transform: ('a -> 'b)) =
        let innerFn =
            match this with
                | Success value -> 
                    let newValue = transform value
                    Success newValue
                | Failure err ->
                    Failure err

        innerFn

    member this.bind transform =
        let innerFn =
            match this with
                | Success value -> 
                    transform value
                | Failure err ->
                    Failure err

        innerFn
    
type Parser<'a> = Parser of (string -> ParserResult<'a * string>) with
    member this.run input =
        let (Parser innerFn) = this
        innerFn input

    member this.map (transform: 'a -> 'b) =
        let innerFn input =
            let result = this.run input
            
            match result with
                | Success (value, remaining) -> Success (transform value, remaining)
                | Failure err -> Failure err

        Parser innerFn

    member this.andThen (parser2: Parser<'b>) =
        let innerFn input =
            let result1 = this.run input
            let result2 = result1.bind (fun (result1, remaining) -> parser2.run remaining)

            match result1, result2 with
            | Success (result1, remaining), Success (result2, remaining2) -> Success ((result1, result2), remaining2)
            | _ -> Failure "Error"

        Parser innerFn

    member this.orElse (parser2: Parser<'a>) =
        let innerFn input =
            let result1 = this.run input
            let result2 = parser2.run input

            match result1, result2 with
            | Success _, _ -> result1
            | _, Success _ -> result2
            | Failure err1, Failure err2 -> Failure (sprintf "%s\n%s" err1 err2)

        Parser innerFn

    static member asParser (x: 'a) =
        let innerFn input =
            Success (x, input)
        Parser innerFn

    static member (.>>.) (parser1: Parser<'a>, parser2: Parser<'b>) =
        parser1.andThen parser2

    static member (<|>) (parser1: Parser<'a>, parser2: Parser<'a>) =
        parser1.orElse parser2

    static member (<!>) (parser: Parser<'a>, transform: ('a -> 'b)) =
        parser.map transform

    member this.apply (parser2: Parser<('a -> 'b)>) =
        (this.andThen parser2)
        <!> (fun (f: 'a, y: 'a ->'b) -> y f)

    static member (<*>) (parser1: Parser<'a>, parser2) =
        parser1.apply parser2

    static member fromChar (charToMatchOn: char) =
        let scanner input = 
            if System.String.IsNullOrWhiteSpace input then
                Failure "No more input"
            else
                let first = input.[0]
                if first = charToMatchOn then
                    let remaining = input.[1..]
                    Success (charToMatchOn, remaining)
                else
                    let error = sprintf "Expecting '%c', got '%c'" charToMatchOn first
                    Failure error

        Parser scanner
    static member fromAnyOf listOfChars =
        listOfChars
        |> List.map Parser.fromChar
        |> List.reduce (fun acc parser -> acc.orElse parser)

    static member choice (listOfParsers: Parser<'a> list) =
        List.reduce (fun (acc: Parser<'a>) (parser: Parser<'a>) -> acc.orElse parser) listOfParsers
    static member lift2 f (xP: Parser<('a -> 'b)>) (yP: Parser<('b -> 'c)>) =
        Parser.asParser f <*> xP <*> yP

    static member sequence (parserList: Parser<'a> list): Parser<'a list> =
        match parserList with
        | [] -> Parser.asParser []
        | parser :: rest ->
            let parserOfList = Parser.sequence rest
            let parserOfList2 = parserOfList.map (fun list -> fun item -> item :: list)
            parser <*> parserOfList2
    static member fromString (str: string) =
        str
        |> List.ofSeq
        |> List.map Parser.fromChar
        |> Parser.sequence
        <!> charListToStr