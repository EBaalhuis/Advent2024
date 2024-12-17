namespace Advent2024

open System

    module Day14Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input14.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        type Robot = {x: int64; y: int64; v_x: int64; v_y: int64 }
        let robots =
            lines |>
            Array.map (fun line -> line.Split ' ') |>
            Array.map (Array.map (fun s -> s.Split ',')) |>
            Array.map (Array.concat) |>
            Array.map (Array.map (Seq.filter (fun c -> Char.IsDigit c || c = '-'))) |>
            Array.map (Array.map (String.Concat)) |>
            Array.map (fun line -> { x=int64 line[0]; y=int64 line[1]; v_x=int64 line[2]; v_y=int64 line[3]})
            
        let max_x = 101L
        let max_y = 103L

        let applySteps (r: Robot) (n: int64) =
            let new_x = (r.x + n * r.v_x + max_x * n) % max_x
            let new_y = (r.y + n * r.v_y + max_y * n) % max_y
            { x=new_x; y=new_y; v_x=r.v_x; v_y=r.v_y}

        let quadrant (r: Robot) =
            if r.x < (max_x / 2L) && r.y < (max_y / 2L) then 1 else
            if r.x > (max_x / 2L) && r.y < (max_y / 2L) then 2 else
            if r.x < (max_x / 2L) && r.y > (max_y / 2L) then 3 else 
            if r.x > (max_x / 2L) && r.y > (max_y / 2L) then 4 else 0
            
        let moved = robots |> Seq.map (fun r -> applySteps r 100)
        let quadrants = moved |> Seq.map quadrant

        let result = 
            quadrants |>
            Seq.countBy id |>
            Seq.filter (fun pair -> fst pair > 0) |>
            Seq.map snd |>
            Seq.reduce (*)

        