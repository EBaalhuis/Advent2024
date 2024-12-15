﻿namespace Advent2024

    module Day10Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input10.txt"

        let lines = readLines filePath |> Array.ofSeq 
        let heights = lines |> Array.map Array.ofSeq |> Array.map (Array.map (fun c -> int (c.ToString())))
        
        let rec getScore (x: int) (y: int) =
            let currentHeight = heights[x][y]
            if currentHeight = 9 then 1
            else 
                let scoreLeft = 
                    if x = 0 then 0
                    else if heights[x-1][y] = currentHeight + 1 then getScore (x-1) y else 0
                let scoreRight = 
                    if x = Seq.length heights - 1 then 0
                    else if heights[x+1][y] = currentHeight + 1 then getScore (x+1) y else 0
                let scoreDown = 
                    if y = 0 then 0
                    else if heights[x][y-1] = currentHeight + 1 then getScore x (y-1) else 0
                let scoreUp = 
                    if y = Seq.length heights[0] - 1 then 0
                    else if heights[x][y+1] = currentHeight + 1 then getScore x (y+1) else 0
                scoreLeft + scoreRight + scoreDown + scoreUp

        let result = 
            Seq.allPairs [0 .. Seq.length heights - 1] [0 .. Seq.length heights[0] - 1] |>
            Seq.filter (fun pair -> heights[fst pair][snd pair] = 0) |>
            Seq.map (fun pair -> getScore (fst pair) (snd pair)) |>
            Seq.reduce (+)
            