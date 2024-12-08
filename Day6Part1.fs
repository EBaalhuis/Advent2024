namespace Advent2024
open System

    module Day6Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input6.txt" |> Array.ofSeq |> Array.map Array.ofSeq

        type Direction = 
            | Up
            | Down
            | Left
            | Right

        let initial_direction = Up
        let initial_x = 
            lines |>
            Array.filter (Array.exists (fun c -> c = '^')) |>
            Array.map (Array.findIndex (fun c -> c = '^')) |>
            Array.reduce max
        let initial_y =
            lines |>
            Array.findIndex (fun arr -> (Array.exists (fun c -> c = '^')) arr)

        let visited = Array2D.init lines[0].Length lines.Length (fun x y -> false)
        let max_x = lines[0].Length - 1
        let max_y = lines.Length - 1

        let is_blocked (direction: Direction) (x: int) (y: int) =
            if direction = Up then
                if y <= 0 then false
                else lines[y-1][x] = '#'
            else if direction = Down then
                if y >= max_y then false
                else lines[y+1][x] = '#'
            else if direction = Left then
                if x <= 0 then false
                else lines[y][x-1] = '#'
            else
                if x >= max_x then false
                else lines[y][x+1] = '#'

        let get_new_direction (direction: Direction) (x: int) (y: int) =
            if (is_blocked direction x y) then
                if direction = Up then Right
                else if direction = Right then Down
                else if direction = Down then Left
                else Up
            else direction

        let rec move (direction: Direction) (x: int) (y: int) =
            if x < 0 || x > max_x || y < 0 || y > max_y then ()
            else 
                visited[x,y] <- true
                let new_direction = get_new_direction direction x y

                let new_x =
                    if new_direction = Left then x - 1
                    else if new_direction = Right then x + 1
                    else x

                let new_y =
                    if new_direction = Up then y - 1
                    else if new_direction = Down then y + 1
                    else y

                move new_direction new_x new_y

        move initial_direction initial_x initial_y

        let result = 
            Seq.allPairs [0 .. (Array2D.length1 visited) - 1] [0 .. (Array2D.length2 visited) - 1] |>
            Seq.map (fun pair -> visited[fst pair, snd pair]) |>
            Seq.filter id |>
            Seq.length
            