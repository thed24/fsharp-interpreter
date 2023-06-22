module Benchmarks

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Tokenizer
open Tokens
open Expression
open Parsers
open FiniteStateMachine

[<MemoryDiagnoser>]
type Benchmarks() =
    let mutable code = ""
    let mutable tokens = []
    
    [<GlobalSetup>]
    member self.Setup() =
        code <- "1 + 2 * 3 - 4 / 5"
        tokens <- [
            { TokenType = NUMBER; Lexeme = "1"; Column = 0; Line = 0 }
            { TokenType = PLUS; Lexeme = "+"; Column = 2; Line = 0 }
            { TokenType = NUMBER; Lexeme = "2"; Column = 4; Line = 0 }
            { TokenType = STAR; Lexeme = "*"; Column = 6; Line = 0 }
            { TokenType = NUMBER; Lexeme = "3"; Column = 8; Line = 0 }
            { TokenType = MINUS; Lexeme = "-"; Column = 10; Line = 0 }
            { TokenType = NUMBER; Lexeme = "4"; Column = 12; Line = 0 }
            { TokenType = SLASH; Lexeme = "/"; Column = 14; Line = 0 }
            { TokenType = NUMBER; Lexeme = "5"; Column = 16; Line = 0 }
        ]

    [<Benchmark>]
    member self.ParserCombinatorTokenizer() =
        tokenize tokenizeUsingParsers code |> ignore

    [<Benchmark>]
    member self.FiniteStateMachineTokenizer() =
        tokenize tokenizeUsingFsm code |> ignore

    [<Benchmark>]
    member self.RecursiveDescentParser() =
        expression tokens |> ignore

[<EntryPoint>]
let main _ =
    BenchmarkRunner.Run<Benchmarks>() |> ignore
    0
