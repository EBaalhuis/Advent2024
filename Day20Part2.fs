namespace Advent2024

    module Day20Part2 =
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
                    
        let cheatLength (sourceX: int) (sourceY: int) (destX: int) (destY: int) =
            abs (sourceX-destX) + abs (sourceY-destY)

        let score (sourceX: int) (sourceY: int) (destX: int) (destY: int) =
            if maze[sourceX][sourceY] = '#' || maze[destX][destY] = '#' then 0L else
            distFromStart[(fst finish), (snd finish)] 
                - distFromStart[sourceX,sourceY] 
                - distFromEnd[destX,destY] 
                - (cheatLength sourceX sourceY destX destY |> int64)

        let limit = 100L
        let cheatTimeAllowed = 20

        let cheatsWithScore =
            update (fst start) (snd start) 0 distFromStart
            update (fst finish) (snd finish) 0 distFromEnd

            Seq.allPairs [0 .. (Array.length maze - 1)] [0 .. (Array.length maze[0] - 1)] |>
            (fun x -> Seq.allPairs x x) |>
            Seq.filter (fun pairs ->
                let sourceX = fst (fst pairs)
                let sourceY = snd (fst pairs)
                let destX = fst (snd pairs)
                let destY = snd (snd pairs)
                cheatLength sourceX sourceY destX destY <= cheatTimeAllowed) |>
            Seq.map (fun pairs -> 
                let sourceX = fst (fst pairs)
                let sourceY = snd (fst pairs)
                let destX = fst (snd pairs)
                let destY = snd (snd pairs)
                (pairs, score sourceX sourceY destX destY))

        let cheatsWithScoreAboveLimit =
            cheatsWithScore |>
            Seq.filter (fun x -> snd x >= limit)

        // improvement: distFromStart[(fst finish), (snd finish)] - distFromStart[x,y] - distFromEnd[x2,y2] - (length of cheat path)
        let result =
            cheatsWithScoreAboveLimit |>
            Seq.map fst |>
            Seq.length