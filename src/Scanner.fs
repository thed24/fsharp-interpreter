module Scanner

type ParserResult<'a> =
    | Success of 'a
    | Failure of string

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

let charListToStr (charList: char list) =
    charList |> List.toArray |> System.String

let run (parser: Parser<'a>) (input: ParserInput) =
    let (Parser innerFn) = parser
    innerFn input

let map (transform: 'a -> 'b) (parser: Parser<'a>) =
    let innerFn (input: ParserInput) =
        let result = run parser input

        match result with
        | Success result -> Success({ value = transform result.value; remainingInput = result.remainingInput })
        | Failure err -> Failure err

    Parser innerFn

let andThen (firstParser: Parser<'a>) (secondParser: Parser<'b>) =
    let innerFn input =
        let result = run firstParser input

        match result with
        | Success result ->
            let result2 = run secondParser result.remainingInput

            match result2 with
            | Success result2 -> Success({ value = (result.value, result2.value); remainingInput = result2.remainingInput })
            | Failure err -> Failure err
        | Failure err -> Failure err

    Parser innerFn

let orElse (firstParser: Parser<'a>) (secondParser: Parser<'a>) =
    let innerFn input =
        let result1 = run firstParser input
        let result2 = run secondParser input

        match result1, result2 with
        | Success _, _ -> result1
        | _, Success _ -> result2
        | Failure err1, Failure err2 -> Failure(sprintf "%s\n%s" err1 err2)

    Parser innerFn

let asParser (x: 'a) =
    let innerFn input = Success({ value = x; remainingInput = input })
    Parser innerFn

let (.>>.) (parser1: Parser<'a>) (parser2: Parser<'b>) = andThen parser1 parser2

let (<|>) (parser1: Parser<'a>) (parser2: Parser<'a>) = orElse parser1 parser2

let (<!>) (parser: Parser<'a>) (transform: 'a -> 'b) = map transform parser

let (.>>) (parser1: Parser<'a>) (parser2: Parser<'a>) =
    parser1 .>>. parser2
    <!> (fun (a, _) -> a)

let (>>.) (parser1: Parser<'a>) (parser2: Parser<'a>) =
    parser1 .>>. parser2
    <!> (fun (_, b) -> b)

let apply (firstParser: Parser<'a>) (secondParser: Parser<'a -> 'b>) =
    (firstParser .>>. secondParser)
    |> map (fun (f, x) -> x f)

let (<*>) (firstParser: Parser<'a>) (secondParser: Parser<'a -> 'b>) = 
    apply firstParser secondParser

let fromChar (charToMatchOn: char) =
    let scanner (input: ParserInput) =
        if System.String.IsNullOrWhiteSpace input.input then
            Failure "No more input"
        else
            let first = input.input.[0]

            if first = charToMatchOn then
                let remaining = input.input.[1..]
                let newColumn = input.column + 1
                Success({ value = charToMatchOn; remainingInput = { input = remaining; column = newColumn; line = input.line } })
            else
                let error = sprintf "Expecting '%c', got '%c'" charToMatchOn first
                Failure error

    Parser scanner

let fromAnyOf (listOfChars: char list) =
    listOfChars
    |> List.map fromChar
    |> List.reduce orElse

let choice (listOfParsers: Parser<'a> list) =
    List.reduce orElse listOfParsers

let lift2 f (xP: Parser<('a -> 'b)>) (yP: Parser<('b -> 'c)>) =
    asParser f <*> xP <*> yP

let rec sequence parserList =
    match parserList with
    | [] -> asParser []
    | parser :: rest ->
        let parserOfList = sequence rest
        let parserOfList2 = parserOfList |> map (fun list -> fun item -> item :: list)
        parser <*> parserOfList2

let fromString (str: string) =
    str |> List.ofSeq |> List.map fromChar |> sequence
    <!> charListToStr

let rec parseZeroOrMore (parser: Parser<'a>) (input: ParserInput) =
    let firstResult = run parser input

    match firstResult with
    | Failure err -> { value = []; remainingInput = input }
    | Success result ->
        let nextResult = parseZeroOrMore parser result.remainingInput
        { value = result.value :: nextResult.value; remainingInput = nextResult.remainingInput }

let rec parseOneOrMore (parser: Parser<'a>) (input: ParserInput) =
    let firstResult = run parser input

    match firstResult with
    | Failure err -> Failure err
    | Success result ->
        let nextResult = parseZeroOrMore parser result.remainingInput
        Success({ value = result.value :: nextResult.value; remainingInput = nextResult.remainingInput })

let many (parser: Parser<'a>) =
    let innerFn input = Success(parseZeroOrMore parser input)

    Parser innerFn

let many1 (parser: Parser<'a>) =
    let innerFn input = parseOneOrMore parser input

    Parser innerFn

let between (leftParser: Parser<'a>) (middleParser: Parser<'a>) (rightParser: Parser<'a>) =
    leftParser >>. middleParser .>> rightParser

