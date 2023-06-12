module Input
open System.IO

let readFileFromPath (path: string) =
    seq {
        use reader = new StreamReader(path)

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let joinLines (input: seq<string>) =
    input |> Seq.fold (fun acc line -> acc + line) ""