namespace Advent2024
open System

    module Day4Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input4.txt" |> List.ofSeq 

        let get_element_at (x: int) (y: int) (lines: string seq) =
            (Array.ofSeq lines)[y] |>
            Array.ofSeq |>
            (fun line -> Array.get line x)

        let is_xmas (start_x: int) (start_y: int) (delta_x: int) (delta_y: int) =
            if start_x + 3 * delta_x < 0 || start_x + 3 * delta_x >= lines.Head.Length then
                false
            else if start_y + 3 * delta_y < 0 || start_y + 3 * delta_y >= lines.Length then
                false
            else
                get_element_at start_x start_y lines = 'X' &&
                get_element_at (start_x + delta_x) (start_y + delta_y) lines = 'M' &&
                get_element_at (start_x + 2 * delta_x) (start_y + 2 * delta_y) lines = 'A' &&
                get_element_at (start_x + 3 * delta_x) (start_y + 3 * delta_y) lines = 'S'

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
            Seq.allPairs [0 .. lines.Head.Length - 1] [0 .. lines.Length - 1] |>
            Seq.map (fun pair -> 
                count_starting_at (fst pair) (snd pair)) |>
            Seq.reduce (+)
            