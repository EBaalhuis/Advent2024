namespace Advent2024

    module Day19Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input19.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        let towels = 
            lines |>
            Seq.filter (fun s -> s.Contains ",") |>
            Seq.head |>
            (fun line -> line.Split ", ")

        let designs = 
            lines |>
            Seq.skip 2

        let rec isPossibleMemoized =
            Memoize.memoizeRec <| fun recF (x: string) -> 
                if x.Length = 0 then true else
            
                towels |>
                Seq.filter (fun t -> x.StartsWith t && recF (x.Substring t.Length)) |>
                Seq.length > 0

        let outcomes = 
            designs |>
            Seq.map (fun design -> isPossibleMemoized design)

        let result = 
            outcomes |>
            Seq.filter id |>
            Seq.length