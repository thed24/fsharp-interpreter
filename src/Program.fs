module Program

open System.IO
open Scanner

let readFileFromPath (path: string) =
    seq {
        use reader = new StreamReader(path)

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let joinLines (input: seq<string>) =
    input |> Seq.fold (fun acc line -> acc + line) ""

[<EntryPoint>]
let main args =
    let parseVar = Parser.fromString "var"
    let scanned = readFileFromPath "input.txt" |> joinLines |> parseVar.run
    
    fprintf stdout "%A" scanned

    0
