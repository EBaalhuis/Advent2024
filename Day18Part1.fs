namespace Advent2024

    module Day18Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input18.txt"

        let lines = readLines filePath |> Array.ofSeq 
        let size = 71
        let bytesToRead = 1024

        let blocked =
            let res = Array2D.create size size false
            let splits = 
                lines |>
                Seq.truncate bytesToRead |>
                Seq.map (fun l -> l.Split ",")
            let pairs = 
                splits |>
                Seq.map (fun split -> (int split[0], int split[1])) 
            for p in pairs do
                res[fst p, snd p] <- true
            res

        let bestSoFar = 
            Array2D.create size size 999999999999L

        let rec update (x: int) (y: int) (newValue: int64) =
            if not blocked[x,y] then
                let currentValue = bestSoFar[x,y]
                if newValue < currentValue then
                    bestSoFar[x,y] <- newValue
                    if x > 0 then update (x-1) y (newValue + 1L)
                    if x < (size-1) then update (x+1) y (newValue + 1L)
                    if y > 0 then update x (y-1) (newValue + 1L)
                    if y < (size-1) then update x (y+1) (newValue + 1L)
        
        let result = 
            blocked |> ignore
            update 0 0 0
            bestSoFar[size-1,size-1]