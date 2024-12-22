namespace Advent2024

open System

    module Day15Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input15.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        let warehouse =
            lines |>
            Array.filter (fun s -> s.Contains('#')) |>
            Array.map Array.ofSeq

        let moves =
            lines |>
            Array.filter (fun s -> not (s.Contains('#'))) |>
            String.Concat |>
            Array.ofSeq |>
            Array.filter (fun c -> c = '^' || c = '>' || c = 'v' || c = '<')

        let rec getNextWall (x: int) (y: int) (dx: int) (dy: int) =
            if warehouse[x][y] = '#' then (x, y) else
            getNextWall (x+dx) (y+dy) (dx) (dy)

        let rec getNextEmpty (x: int) (y: int) (dx: int) (dy: int) =
            if x < 0 || y < 0 || x >= Array.length warehouse || y >= Array.length warehouse[0] then (999999,999999) else
            if warehouse[x][y] = '.' then (x, y) else
            getNextEmpty (x+dx) (y+dy) (dx) (dy)

        let getDx (c: char) =
            if c = '^' then -1 else
            if c = 'v' then 1 else
            0

        let getDy (c: char) =
            if c = '<' then -1 else
            if c = '>' then 1 else
            0

        let rec doMoves (moves: char array) (x: int) (y: int) =
            if Array.length moves = 0 then () else
                let c = Array.head moves
                let newMoves = Array.tail moves
                let nextEmpty = getNextEmpty (x + getDx c) (y + getDy c) (getDx c) (getDy c)
                let nextWall = getNextWall (x + getDx c) (y + getDy c) (getDx c) (getDy c)
                if abs (fst nextWall - x) + abs (snd nextWall - y) < abs (fst nextEmpty - x) + abs (snd nextEmpty - y) then 
                    doMoves newMoves x y
                else
                    warehouse[fst nextEmpty][snd nextEmpty] <- 'O'
                    warehouse[(x + getDx c)][(y + getDy c)] <- '.'
                    doMoves newMoves (x + (getDx c)) (y + (getDy c))

        let initialRobotPosition =
            Seq.allPairs [0 .. Array.length warehouse - 1] [0 .. Array.length warehouse[0] - 1] |>
            Seq.filter (fun pair -> warehouse[fst pair][snd pair] = '@') |>
            Seq.head

        let result =
            warehouse[fst initialRobotPosition][snd initialRobotPosition] <- '.'
            doMoves moves (fst initialRobotPosition) (snd initialRobotPosition)
            
            Seq.allPairs [0 .. Array.length warehouse - 1] [0 .. Array.length warehouse[0] - 1] |>
            Seq.filter (fun pair -> warehouse[fst pair][snd pair] = 'O') |>
            Seq.map (fun pair -> 100 * (fst pair) + snd pair) |>
            Seq.reduce (+)
        