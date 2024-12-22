namespace Advent2024

open System

    module Day16Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input16.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        let maze =
            lines |>
            Array.map Array.ofSeq

        type Direction = 
            | Up
            | Down
            | Left
            | Right

        type State = {x: int; y: int; d: Direction}

        let directionToInt (d: Direction) =
            match d with
            | Direction.Up -> 3
            | Direction.Left -> 2
            | Direction.Down -> 1
            | Direction.Right -> 0
        
        let getDx (d: Direction) =
            if d = Direction.Up then -1 else 
            if d = Direction.Down then 1 else 
            0

        let getDy (d: Direction) =
            if d = Direction.Left then -1 else 
            if d = Direction.Right then 1 else 
            0

        let turnLeft (d: Direction) =
            match d with
            | Direction.Up -> Direction.Left
            | Direction.Left -> Direction.Down
            | Direction.Down -> Direction.Right
            | Direction.Right -> Direction.Up

        let turnRight (d: Direction) =
            match d with
            | Direction.Up -> Direction.Right
            | Direction.Right -> Direction.Down
            | Direction.Down -> Direction.Left
            | Direction.Left -> Direction.Up

        let start =
            Seq.allPairs [0 .. Array.length maze - 1] [0 .. Array.length maze[0] - 1] |>
            Seq.filter (fun pair -> maze[fst pair][snd pair] = 'S') |>
            Seq.head

        let finish =
            Seq.allPairs [0 .. Array.length maze - 1] [0 .. Array.length maze[0] - 1] |>
            Seq.filter (fun pair -> maze[fst pair][snd pair] = 'E') |>
            Seq.head

        let bestSoFar = 
            Array3D.create (Array.length maze) (Array.length maze[0]) 4 999999999999L

        let rec update (x: int) (y: int) (d: Direction) (newValue: int64) =
            if not (maze[x][y] = '#') then
                let dir = directionToInt d
                let currentValue = bestSoFar[x,y,dir]
                if newValue < currentValue then
                    bestSoFar[x,y,dir] <- newValue
                    update x y (turnLeft d) (newValue + 1000L)
                    update x y (turnRight d) (newValue + 1000L)
                    let nextX = x + getDx d
                    let nextY = y + getDy d
                    if nextX >= 0 && nextX < Array.length maze && nextY >= 0 && nextY < Array.length maze[0] then
                        update nextX nextY d (newValue + 1L)
                

        let result =
            update (fst start) (snd start) Direction.Right 0
            [0 .. 3] |>
            Seq.map (fun x -> bestSoFar[(fst finish),(snd finish),x]) |>
            Seq.reduce min
        
