namespace Advent2024

    module Day11Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input11.txt"

        let lines = readLines filePath |> Array.ofSeq 
        let initialStones = lines[0].Split " " |> Seq.map int
        
        let rec applyRule (stone: int64) (blinks: int) =
            if blinks = 0 then 1 else
            if stone = 0 then applyRule 1 (blinks - 1) else
            let stoneString = stone.ToString()
            if stoneString.Length % 2 = 0 then
                let leftPart = stoneString.Substring(0, stoneString.Length / 2)
                let rightPart = stoneString.Substring(stoneString.Length / 2, stoneString.Length / 2)
                applyRule (int leftPart) (blinks - 1) + applyRule (int rightPart) (blinks - 1)
            else
                applyRule (stone * 2024L) (blinks - 1)

        let result = 
            initialStones |>
            Seq.map (fun stone -> applyRule stone 25) |>
            Seq.reduce (+)