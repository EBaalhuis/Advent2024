namespace Advent2024

open System

    module Day15Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input15.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        let warehouse =
            lines |>
            Array.filter (fun s -> s.Contains('#')) |>
            Array.map (fun s -> (s.Replace("#", "##"))) |>
            Array.map (fun s -> (s.Replace("O", "[]"))) |>
            Array.map (fun s -> (s.Replace(".", ".."))) |>
            Array.map (fun s -> (s.Replace("@", "@."))) |>
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

        let rec canMove (move: char) (x: int) (y: int) =
            let dx = getDx move
            let dy = getDy move
            let nextChar = warehouse[x+dx][y+dy]
            if nextChar = '.' then true else
            if nextChar = '#' then false else
            if dx = 0 then canMove move x (y+dy) else
            if nextChar = '[' then canMove move (x+dx) y && canMove move (x+dx) (y+1) else
            canMove move (x+dx) y && canMove move (x+dx) (y-1)

        let rec executeMove (move: char) (x: int) (y: int) =
            let dx = getDx move
            let dy = getDy move
            let nextChar = warehouse[x+dx][y+dy]
            if dy = 0 then
                if nextChar = '[' then
                    executeMove move (x+dx) (y+dy) 
                    executeMove move (x+dx) (y+dy+1) 
                else if nextChar = ']' then
                    executeMove move (x+dx) (y+dy)
                    executeMove move (x+dx) (y+dy-1)
            else
                if not (nextChar = '.') then 
                    executeMove move (x+dx) (y+dy) 
            let prev = warehouse[x][y]
            warehouse[x][y] <- warehouse[x+dx][y+dy]
            warehouse[x+dx][y+dy] <- prev

        let rec doMoves (moves: char array) (x: int) (y: int) =
            if Array.length moves = 0 then () else
                let move = Array.head moves
                let newMoves = Array.tail moves
                if not (canMove move x y) then 
                    doMoves newMoves x y
                else
                    executeMove move x y
                    doMoves newMoves (x + (getDx move)) (y + (getDy move))

        let initialRobotPosition =
            Seq.allPairs [0 .. Array.length warehouse - 1] [0 .. Array.length warehouse[0] - 1] |>
            Seq.filter (fun pair -> warehouse[fst pair][snd pair] = '@') |>
            Seq.head

        let result =
            warehouse[fst initialRobotPosition][snd initialRobotPosition] <- '.'
            doMoves moves (fst initialRobotPosition) (snd initialRobotPosition)
            
            Seq.allPairs [0 .. Array.length warehouse - 1] [0 .. Array.length warehouse[0] - 1] |>
            Seq.filter (fun pair -> warehouse[fst pair][snd pair] = '[') |>
            Seq.map (fun pair -> 100 * (fst pair) + snd pair) |>
            Seq.reduce (+)
        