namespace Advent2024
    module Day1Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input1.txt" |> List.ofSeq

        let takeLeft (line: string) = line.Split ' ' |> Array.head
        let takeRight (line: string) = line.Split ' ' |> Array.last

        let a = lines |> Seq.map(takeLeft) |> Seq.map int
        let b = lines |> Seq.map(takeRight) |> Seq.map int

        let counts = Seq.countBy id b
        let z = a |> Seq.map (fun x -> Seq.filter(fun y -> fst y = x) counts) 
        let z2 = z |> Seq.map (fun x -> 
            if Seq.isEmpty x then
                0
            else
                Seq.head x |> snd)

        let scores = Seq.map2 (fun x y -> x * y) a z2
        let result = Seq.reduce (fun x y -> x + y) scores