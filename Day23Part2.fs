namespace Advent2024

    module Day23Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input23.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        let getEdges (line: string) : string array =
            line.Split "-"

        let edgeFromLine (line: string) =
            let split = line.Split "-"
            (split[0], split[1])

        let graph = 
            let edges =
                lines |>
                Seq.map getEdges

            let vertices =
                edges |>
                Seq.concat |>
                Seq.distinct

            let getNeighbors (v: string) =
                edges |>
                Seq.filter (fun edge -> edge[0] = v || edge[1] = v) |>
                Seq.concat |>
                Seq.filter (fun w -> not (w = v)) |>
                Set.ofSeq

            vertices |>
            Seq.map (fun v -> (v, getNeighbors v)) |>
            Map.ofSeq

        let rec getMaximalCliques (R: seq<string>) (P: seq<string>) (X: seq<string>) : seq<seq<string>> =
            if Seq.isEmpty P && Seq.isEmpty X then Seq.singleton R else

            let getNewR (v: string) = Seq.append R (Seq.singleton v)
            let getNewP (v: string) =     
                P |>
                Seq.filter (fun u -> graph[v].Contains u) |>
                Seq.filter (fun u -> u > v)
            let getNewX (v: string) =
                P |>
                Seq.filter (fun u -> u < v) |>
                Seq.append X |>
                Seq.filter (fun u -> graph[v].Contains u) 

            P |>
            Seq.map (fun v -> getMaximalCliques (getNewR v) (getNewP v) (getNewX v)) |>
            Seq.concat

        let result = 
            let maximalCliques = getMaximalCliques Seq.empty (graph.Keys |> Seq.sort) Seq.empty
            let largestCliqueSize = maximalCliques |> Seq.map Seq.length |> Seq.max
            let maximalCliqueWithLargestSize = maximalCliques |> Seq.filter (fun c -> Seq.length c = largestCliqueSize) |> Seq.head
            maximalCliqueWithLargestSize |> Seq.sort |> String.concat ","