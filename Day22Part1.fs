namespace Advent2024

    module Day22Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input22.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        let m = 16777216L

        let getNextNumber (s: int64) =
            let a = s * 64L
            let b = (s ^^^ a) % m
            let c = b / 32L
            let d = (b ^^^ c) % m
            let e = d * 2048L
            let f = (d ^^^ e) % m
            f

        let repeat n =
            Seq.init n (fun _ u -> getNextNumber u) |>
            Seq.reduce (>>)

        let result = 
            lines |>
            Seq.map int64 |>
            Seq.map (repeat 2000) |>
            Seq.reduce (+)