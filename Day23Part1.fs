namespace Advent2024

    module Day23Part1 =
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

        let isTriangle (vertices: string * string * string) =
            let (v1, v2, v3) = vertices
            graph[v1].Contains v2 &&
            graph[v2].Contains v3 &&
            graph[v3].Contains v1

        let triangles = 
            (lines |> Seq.map edgeFromLine) |>
            Seq.allPairs graph.Keys |>
            Seq.map (fun x -> (fst x, fst (snd x), snd (snd x))) |> 
            Seq.filter (fun x -> isTriangle x)

        let anyVertexStartsWithT (vertices: string * string * string) =
            let (v1, v2, v3) = vertices
            v1.StartsWith "t" ||
            v2.StartsWith "t" ||
            v3.StartsWith "t"
        
        let result = 
            triangles |>
            Seq.filter anyVertexStartsWithT |>
            Set.ofSeq |>
            Seq.length |>
            (fun x -> x / 3) // each triangle is counted 3 times, once for each of its edges