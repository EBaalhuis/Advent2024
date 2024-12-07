namespace Advent2024
open System

    module Day4Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input4.txt" |> Array.ofSeq |> Array.map Array.ofSeq

        let is_xmas (start_x: int) (start_y: int) (delta_x: int) (delta_y: int) =
            if start_x + 3 * delta_x < 0 || start_x + 3 * delta_x >= lines[0].Length then
                false
            else if start_y + 3 * delta_y < 0 || start_y + 3 * delta_y >= lines.Length then
                false
            else
                lines[start_y][start_x] = 'X' &&
                lines[start_y + delta_y][start_x + delta_x] = 'M' &&
                lines[start_y + 2 * delta_y][start_x + 2 * delta_x] = 'A' &&
                lines[start_y + 3 * delta_y][start_x + 3 * delta_x] = 'S'

        let count_starting_at (start_x: int) (start_y: int) =
            (if (is_xmas start_x start_y 0 1) then 1 else 0) +
            (if (is_xmas start_x start_y 0 -1) then 1 else 0) + 
            (if (is_xmas start_x start_y 1 0) then 1 else 0) +
            (if (is_xmas start_x start_y 1 1) then 1 else 0) + 
            (if (is_xmas start_x start_y 1 -1) then 1 else 0) +
            (if (is_xmas start_x start_y -1 0) then 1 else 0) + 
            (if (is_xmas start_x start_y -1 1) then 1 else 0) +
            (if (is_xmas start_x start_y -1 -1) then 1 else 0)

        let result = 
            Seq.allPairs [0 .. lines[0].Length - 1] [0 .. lines.Length - 1] |>
            Seq.map (fun pair -> 
                count_starting_at (fst pair) (snd pair)) |>
            Seq.reduce (+)
            