namespace Advent2024

    module Day19Part2 =
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

        let rec countOptionsMemoized =
            Memoize.memoizeRec <| fun recF (x: string) -> 
                if x.Length = 0 then 1L else
            
                towels |>
                Seq.map (fun t -> 
                    if x.StartsWith t then recF (x.Substring t.Length) else 0L) |>
                Seq.reduce (+)

        let outcomes = 
            designs |>
            Seq.map (fun design -> countOptionsMemoized design)

        let result = 
            outcomes |>
            Seq.reduce (+)