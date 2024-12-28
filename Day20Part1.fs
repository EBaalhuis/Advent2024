namespace Advent2024

    module Day20Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input20.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        let maze =
            lines |>
            Array.map Array.ofSeq

        let start =
            Seq.allPairs [0 .. Array.length maze - 1] [0 .. Array.length maze[0] - 1] |>
            Seq.filter (fun pair -> maze[fst pair][snd pair] = 'S') |>
            Seq.head

        let finish =
            Seq.allPairs [0 .. Array.length maze - 1] [0 .. Array.length maze[0] - 1] |>
            Seq.filter (fun pair -> maze[fst pair][snd pair] = 'E') |>
            Seq.head

        let distFromStart = 
            Array2D.create (Array.length maze) (Array.length maze[0]) 999999999999L

        let distFromEnd = 
            Array2D.create (Array.length maze) (Array.length maze[0]) 999999999999L

        let rec update (x: int) (y: int) (newValue: int64) (dists: int64[,]) =
            if not (maze[x][y] = '#') then
                let currentValue = dists[x,y]
                if newValue < currentValue then
                    dists[x,y] <- newValue
                    if (x-1) >= 0 then
                        update (x-1) y (newValue + 1L) dists
                    if (x+1) < Array.length maze then
                        update (x+1) y (newValue + 1L) dists
                    if (y-1) >= 0 then
                        update x (y-1) (newValue + 1L) dists
                    if (y+1) < Array.length maze[0] then
                        update x (y+1) (newValue + 1L) dists
                    
        let directions = [(0,-2); (1,1); (1,-1); (-1,1); (-1,-1); (2,0); (-2,0); (0,2)]
        
        let score (sourceX: int) (sourceY: int) (direction: int * int) =
            let destX = sourceX + fst direction
            let destY = sourceY + snd direction
            if destX < 0 || destY < 0 || destX >= Array.length maze || destY >= Array.length maze[0] then 0L else
            if maze[sourceX][sourceY] = '#' || maze[destX][destY] = '#' then 0L else
            distFromStart[(fst finish), (snd finish)] - distFromStart[sourceX,sourceY] - distFromEnd[destX,destY] - 2L 

        let middles (sourceX: int) (sourceY: int) (direction: int * int) =
            match direction with
            | (2,0) -> [(sourceX + 1, sourceY)]
            | (-2,0) -> [(sourceX - 1, sourceY)]
            | (0,2) -> [(sourceX, sourceY + 1)]
            | (0,-2) -> [(sourceX, sourceY - 1)]
            | (1,1) -> [(sourceX, sourceY + 1); (sourceX + 1, sourceY)]
            | (1,-1) -> [(sourceX, sourceY - 1); (sourceX + 1, sourceY)]
            | (-1,1) -> [(sourceX, sourceY + 1); (sourceX - 1, sourceY)]
            | (-1,-1) -> [(sourceX, sourceY - 1); (sourceX - 1, sourceY)]
            | _ -> []
        
        let limit = 100L

        let cheatsWithScore =
            update (fst start) (snd start) 0 distFromStart
            update (fst finish) (snd finish) 0 distFromEnd

            Seq.allPairs [0 .. (Array.length maze - 1)] [0 .. (Array.length maze[0] - 1)] |>
            Seq.allPairs directions |>
            Seq.map (fun pairs -> 
                let x = fst (snd pairs)
                let y = snd (snd pairs)
                (pairs, score x y (fst pairs)))

        let cheatsWithScoreAboveLimit =
            cheatsWithScore |>
            Seq.filter (fun x -> snd x >= limit)

        // try each cheat
        let result =
            cheatsWithScoreAboveLimit |>
            Seq.map fst |>
            // determine distinct cheats
            Seq.map (fun pairs ->
                let x = fst (snd pairs)
                let y = snd (snd pairs)
                middles x y (fst pairs)) |>
            Seq.concat |>
            Seq.length