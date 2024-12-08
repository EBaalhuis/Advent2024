namespace Advent2024
open System

    module Day6Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input6.txt"

        let lines = readLines filePath |> Array.ofSeq |> Array.map Array.ofSeq

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

        let is_blocked (direction: Direction) (x: int) (y: int) (lines: char array array) =
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

        let rec get_new_direction (direction: Direction) (x: int) (y: int) (lines: char array array) =
            if (is_blocked direction x y lines) then
                let next_direction =
                    if direction = Up then Right
                    else if direction = Right then Down
                    else if direction = Down then Left
                    else Up
                get_new_direction next_direction x y lines
            else direction

        let rec move (direction: Direction) (x: int) (y: int) =
            if x < 0 || x > max_x || y < 0 || y > max_y then ()
            else 
                visited[x,y] <- true
                let new_direction = get_new_direction direction x y lines

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

        let lines_with_new_obstacle (x: int) (y: int) =
            let result = readLines filePath |> Array.ofSeq |> Array.map Array.ofSeq
            result[y][x] <- '#'
            result

        type State = { Dir: Direction; X: int; Y: int }
        
        let rec is_a_loop (direction: Direction) (x: int) (y: int) (seen_states: State Set) (lines: char array array) =
            if x < 0 || x > max_x || y < 0 || y > max_y then false
            else 
                let current_state = { Dir = direction; X = x; Y = y}
                if seen_states.Contains current_state then true
                else
                    let new_direction = get_new_direction direction x y lines

                    let new_x =
                        if new_direction = Left then x - 1
                        else if new_direction = Right then x + 1
                        else x

                    let new_y =
                        if new_direction = Up then y - 1
                        else if new_direction = Down then y + 1
                        else y

                    let new_seen_states =
                        Set.union seen_states (Set.singleton current_state)

                    is_a_loop new_direction new_x new_y new_seen_states lines

        let result = 
            Seq.allPairs [0 .. (Array2D.length1 visited) - 1] [0 .. (Array2D.length2 visited) - 1] |>
            Seq.filter (fun pair -> not (fst pair = initial_x) || not (snd pair = initial_y)) |>
            Seq.map (fun pair -> (visited[fst pair, snd pair]), pair) |>
            Seq.filter (fun pair -> fst pair) |>
            Seq.map (fun pair -> snd pair) |>
            Seq.map (fun pair -> (lines_with_new_obstacle (fst pair) (snd pair))) |>
            Seq.map (is_a_loop initial_direction initial_x initial_y Set.empty) |>
            Seq.filter id |>
            Seq.length
            