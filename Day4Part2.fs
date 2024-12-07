namespace Advent2024
open System

    module Day4Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input4.txt" |> Array.ofSeq |> Array.map Array.ofSeq

        let is_mas (x: int) (y: int) (delta_x: int) (delta_y: int) =
            lines[y][x] = 'A' &&
            (
                (lines[y - delta_y][x - delta_x] = 'M' && lines[y + delta_y][x + delta_x] = 'S') ||
                (lines[y - delta_y][x - delta_x] = 'S' && lines[y + delta_y][x + delta_x] = 'M')
            )

        let diagonal1_mas (x: int) (y: int) =
            is_mas x y 1 1

        let diagonal2_mas (x: int) (y: int) =
            is_mas x y -1 1

        let result = 
            Seq.allPairs [1 .. lines[0].Length - 2] [1 .. lines.Length - 2] |>
            Seq.map (fun pair -> 
                diagonal1_mas (fst pair) (snd pair) &&
                diagonal2_mas (fst pair) (snd pair)) |>
            Seq.filter id |>
            Seq.length
        