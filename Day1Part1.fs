namespace Advent2024
    module Day1Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input1.txt"

        let takeLeft (line: string) = line.Split ' ' |> Array.head
        let takeRight (line: string) = line.Split ' ' |> Array.last

        let a = lines |> Seq.map(takeLeft) |> Seq.sort |> Seq.map int
        let b = lines |> Seq.map(takeRight) |> Seq.sort |> Seq.map int

        let diffs = Seq.map2 (fun x y -> abs(x - y)) a b

        let result = Seq.reduce (fun x y -> x + y) diffs