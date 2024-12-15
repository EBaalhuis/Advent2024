namespace Advent2024

open System.Collections.Generic

    module Day12Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input12.txt"

        let lines = readLines filePath |> Array.ofSeq 
        let plants = lines |> Array.map Array.ofSeq

        let max_x = plants.Length - 1
        let max_y = plants[0].Length - 1

        let visited = Array2D.zeroCreate<bool> (max_x + 1) (max_y + 1)
        let region = Array2D.zeroCreate<int> (max_x + 1) (max_y + 1)
        let perimeters = new Dictionary<int, int>()

        let rec visit (x: int) (y: int) (currentRegion: int) =
            if not visited[x,y] then
                visited[x,y] <- true
                region[x,y] <- currentRegion
                if x > 0 && plants[x-1][y] = plants[x][y] then visit (x-1) y currentRegion
                if x < max_x && plants[x+1][y] = plants[x][y] then visit (x+1) y currentRegion
                if y > 0 && plants[x][y-1] = plants[x][y] then visit x (y-1) currentRegion
                if y < max_y && plants[x][y+1] = plants[x][y] then visit x (y+1) currentRegion 


        let result = 
            let a = 
                Seq.allPairs [0 .. max_x] [0 .. max_y] |>
                Seq.map (fun pair -> visit (fst pair) (snd pair) (fst pair + (max_x + 1) * snd pair)) |> 
                Seq.map (fun () -> 0) |>
                Seq.reduce (+)

            let b = 
                Seq.allPairs [0 .. max_x] [0 .. max_y] |>
                Seq.map (fun pair ->
                    let x = fst pair
                    let y = snd pair
                    let c1 = 
                        if x > 0 && plants[x-1][y] = plants[x][y] then 0 
                        else if y = 0 then 1 
                        else if not (plants[x][y-1] = plants[x][y]) then 1
                        else if x = 0 then 0
                        else if plants[x-1][y-1] = plants[x][y] then 1
                        else 0
                    let c2 = 
                        if x < max_x && plants[x+1][y] = plants[x][y] then 0 
                        else if y = 0 then 1 
                        else if not (plants[x][y-1] = plants[x][y]) then 1
                        else if x = max_x then 0
                        else if plants[x+1][y-1] = plants[x][y] then 1
                        else 0
                    let c3 = 
                        if y > 0 && plants[x][y-1] = plants[x][y] then 0 
                        else if x = 0 then 1 
                        else if not (plants[x-1][y] = plants[x][y]) then 1
                        else if y = 0 then 0
                        else if plants[x-1][y-1] = plants[x][y] then 1
                        else 0
                    let c4 = 
                        if y < max_y && plants[x][y+1] = plants[x][y] then 0 
                        else if x = 0 then 1 
                        else if not (plants[x-1][y] = plants[x][y]) then 1
                        else if y = max_y then 0
                        else if plants[x-1][y+1] = plants[x][y] then 1
                        else 0 
                    if perimeters.ContainsKey(region[x,y]) then
                        perimeters[region[x,y]] <- perimeters[region[x,y]]+c1+c2+c3+c4
                    else
                        perimeters[region[x,y]] <- c1+c2+c3+c4) |> 
                Seq.map (fun () -> 0) |>
                Seq.reduce (+)

            let areas = region |> Seq.cast<int> |> Seq.countBy id

            a + b + (areas |>
            Seq.map (fun pair -> perimeters[fst pair] * snd pair) |>
            Seq.reduce (+))