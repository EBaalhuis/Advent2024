namespace Advent2024
open System

    module Day4Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input4.txt" |> List.ofSeq 

        let get_element_at (x: int) (y: int) (lines: string seq) =
            (Array.ofSeq lines)[y] |>
            Array.ofSeq |>
            (fun line -> Array.get line x)

        let is_mas (x: int) (y: int) (delta_x: int) (delta_y: int) (lines: string seq) =
            get_element_at x y lines = 'A' &&
            (
                (get_element_at (x - delta_x) (y - delta_y) lines = 'M' && get_element_at (x + delta_x) (y + delta_y) lines = 'S') ||
                (get_element_at (x - delta_x) (y - delta_y) lines = 'S' && get_element_at (x + delta_x) (y + delta_y) lines = 'M')
            )

        let diagonal1_mas (x: int) (y: int) (lines: string seq) =
            is_mas x y 1 1 lines

        let diagonal2_mas (x: int) (y: int) (lines: string seq) =
            is_mas x y -1 1 lines

        let result = 
            Seq.allPairs [1 .. lines.Head.Length - 2] [1 .. lines.Length - 2] |>
            Seq.map (fun pair -> 
                diagonal1_mas (fst pair) (snd pair) lines &&
                diagonal2_mas (fst pair) (snd pair) lines) |>
            Seq.filter id |>
            Seq.length
        