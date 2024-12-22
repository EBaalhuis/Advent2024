namespace Advent2024

    module Day18Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input18.txt"

        let lines = readLines filePath |> Array.ofSeq 
        let size = 71
        
        let getIndex (x: int) (y: int) =
            x + size * y

        let maxIndex = getIndex (size - 1) (size - 1)
        let topRight = maxIndex + 1
        let bottomLeft = maxIndex + 2

        let parent = [0 .. bottomLeft] |> Array.ofList
        let rank = Array.create (bottomLeft + 1) 0

        let rec find (x: int) = 
            if not (parent[x] = x) then parent[x] <- find parent[x]
            parent[x]

        let union (x: int) (y: int) =
            let xRoot = find x
            let yRoot = find y
            if not (xRoot = yRoot) then
                if rank[xRoot] < rank[yRoot] then
                    parent[xRoot] <- yRoot
                else if rank[xRoot] > rank[yRoot] then
                    parent[yRoot] <- xRoot
                else
                    parent[yRoot] <- xRoot
                    rank[xRoot] <- rank[xRoot] + 1

        let rec getResult (count: int) =
            if find topRight = find bottomLeft then
                count
            else
                let x = (lines[count].Split ",")[0] |> int
                let y = (lines[count].Split ",")[1] |> int
                let index = getIndex x y
                if y = 0 || x = size - 1 then union index bottomLeft
                else
                    union index (getIndex (x+1) y)
                    union index (getIndex x (y-1))
                    union index (getIndex (x+1) (y-1))
                getResult (count + 1)

        let result = 
            for x in [0 .. (size - 1)] do   
                union  topRight (getIndex x (size - 1))
            for y in [0 .. (size - 1)] do
                union topRight (getIndex 0 y)

            lines[(getResult 0) - 1]
                    
