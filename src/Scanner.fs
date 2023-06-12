module Scanner

open Tokens

let charListToStr charList =
    charList |> List.toArray |> System.String

type ParserResult<'a> =
    | Success of 'a
    | Failure of string

    member this.map(transform: ('a -> 'b)) =
        let innerFn =
            match this with
            | Success value ->
                let newValue = transform value
                Success newValue
            | Failure err -> Failure err

        innerFn

    member this.bind transform =
        let innerFn =
            match this with
            | Success value -> transform value
            | Failure err -> Failure err

        innerFn

type ParserInput = {
    input: string
    line: int
    column: int
}

type ParserOutput<'a> = {
    value: 'a
    remainingInput: ParserInput
}

type Parser<'a> =
    | Parser of (ParserInput -> ParserResult<ParserOutput<'a>>)

    member this.run input =
        let (Parser innerFn) = this
        innerFn input

    member this.map(transform: 'a -> 'b) =
        let innerFn input =
            let result = this.run input

            match result with
            | Success(result) -> Success({ value = transform result.value; remainingInput = result.remainingInput })
            | Failure err -> Failure err

        Parser innerFn

    member this.andThen(parser2: Parser<'b>) =
        let innerFn input =
            let result = this.run input

            match result with
            | Success(result) ->
                let result2 = parser2.run result.remainingInput

                match result2 with
                | Success(result2) -> Success({ value = (result.value, result2.value); remainingInput = result2.remainingInput })
                | Failure err -> Failure err
            | Failure err -> Failure err

        Parser innerFn

    member this.orElse(parser2: Parser<'a>) =
        let innerFn input =
            let result1 = this.run input
            let result2 = parser2.run input

            match result1, result2 with
            | Success _, _ -> result1
            | _, Success _ -> result2
            | Failure err1, Failure err2 -> Failure(sprintf "%s\n%s" err1 err2)

        Parser innerFn

    static member asParser(x: 'a) =
        let innerFn input = Success({ value = x; remainingInput = input })
        Parser innerFn

    static member (.>>.)(parser1: Parser<'a>, parser2: Parser<'b>) = parser1.andThen parser2

    static member (<|>)(parser1: Parser<'a>, parser2: Parser<'a>) = parser1.orElse parser2

    static member (<!>)(parser: Parser<'a>, transform: ('a -> 'b)) = parser.map transform

    static member (.>>)(parser1: Parser<'a>, parser2: Parser<'b>) =
        parser1 .>>. parser2
        <!> (fun (a,b) -> a)

    static member (>>.) (parser1: Parser<'a>, parser2: Parser<'b>) =
        parser1 .>>. parser2
        <!> (fun (a,b) -> b)

    member this.apply(parser2: Parser<('a -> 'b)>) =
        (this.andThen parser2) <!> (fun (f: 'a, y: 'a -> 'b) -> y f)

    static member (<*>)(parser1: Parser<'a>, parser2) = parser1.apply parser2

    static member fromChar<'a>(charToMatchOn: char, ?newLine: bool) =
        let scanner (input: ParserInput) =
            if System.String.IsNullOrWhiteSpace input.input then
                Failure "No more input"
            else
                let newLine = defaultArg newLine false

                let first = input.input.[0]

                if first = charToMatchOn then
                    let remaining = input.input.[1..]
                    let newColumn = input.column + 1
                    let newLineNum = if newLine = true then (input.line + 1) else input.line
                    Success({ value = charToMatchOn; remainingInput = { input = remaining; column = newColumn; line = newLineNum } })
                else
                    let error = sprintf "Expecting '%c', got '%c'" charToMatchOn first
                    Failure error

        Parser scanner

    static member fromAnyOf<'a> listOfChars =
        listOfChars
        |> List.map Parser.fromChar<'a>
        |> List.reduce (fun acc parser -> acc.orElse parser)

    static member choice(listOfParsers: Parser<'a> list) =
        List.reduce (fun (acc: Parser<'a>) (parser: Parser<'a>) -> acc.orElse parser) listOfParsers

    static member lift2 f (xP: Parser<('a -> 'b)>) (yP: Parser<('b -> 'c)>) = Parser.asParser f <*> xP <*> yP

    static member sequence(parserList: Parser<'a> list) : Parser<'a list> =
        match parserList with
        | [] -> Parser.asParser []
        | parser :: rest ->
            let parserOfList = Parser.sequence rest
            let parserOfList2 = parserOfList.map (fun list -> fun item -> item :: list)
            parser <*> parserOfList2

    static member fromString<'a>(str: string) =
        str |> List.ofSeq |> List.map Parser.fromChar<'a> |> Parser.sequence
        <!> charListToStr

    member this.parseZeroOrMore input =
        let firstResult = this.run input

        match firstResult with
        | Failure err -> { value = []; remainingInput = input}
        | Success(result) ->
            let nextResult = this.parseZeroOrMore result.remainingInput
            { value = result.value :: nextResult.value; remainingInput = nextResult.remainingInput }

    member this.parseOneOrMore input =
        let firstResult = this.run input

        match firstResult with
        | Failure err -> Failure err
        | Success(result) ->
            let nextResult = this.parseZeroOrMore result.remainingInput
            Success({ value = result.value :: nextResult.value; remainingInput = nextResult.remainingInput })

    member this.many =
        let innerFn input = Success(this.parseZeroOrMore input)

        Parser innerFn

    member this.many1 =
        let innerFn input = this.parseOneOrMore input

        Parser innerFn

    member this.between parser1 parser2 =
        parser1 >>. this .>> parser2