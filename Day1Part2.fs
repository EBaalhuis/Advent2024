namespace Advent2024
    module Day1Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input1.txt"

        let takeLeft (line: string) = line.Split ' ' |> Array.head
        let takeRight (line: string) = line.Split ' ' |> Array.last

        let a = List.ofSeq(lines) |> List.map(takeLeft) |> List.map int
        let b = List.ofSeq(lines) |> List.map(takeRight) |> List.map int

        let counts = List.countBy id b
        let z = a |> List.map (fun x -> List.filter(fun y -> fst y = x) counts) 
        let z2 = z |> List.map (fun x -> 
            if x.IsEmpty then
                0
            else
                List.head x |> snd)

        let scores = List.map2 (fun x y -> x * y) a z2
        let result = List.reduce (fun x y -> x + y) scores