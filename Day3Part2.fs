namespace Advent2024
open System

    module Day3Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input3.txt" |> List.ofSeq 

        let stringContainsOnlyDigits (s:string) = s |> Seq.forall Char.IsDigit

        let result = 
            lines |> 
            Seq.reduce (fun x y -> x + y) |>
            (fun s -> s.Split "do()") |>
            Seq.map (fun s -> s.Split "don't()") |>
            Seq.map Seq.head |>
            Seq.map (fun s -> s.Split "mul(") |>
            Seq.concat |>
            Seq.map (fun s -> s.Split ")") |>
            Seq.map Seq.head |>
            Seq.map (fun s -> s.Split ",") |>
            Seq.filter (fun sq -> sq.Length = 2) |>
            Seq.map (Seq.filter (fun elem -> stringContainsOnlyDigits elem)) |>
            Seq.filter (fun sq -> (Seq.length sq) = 2) |>
            Seq.map (Seq.filter (fun elem -> int elem >= 0 && int elem <= 999)) |>
            Seq.filter (fun sq -> (Seq.length sq) = 2) |>
            Seq.map (Seq.map int) |>
            Seq.map (Seq.reduce (fun x y -> x * y)) |>
            Seq.reduce (fun x y -> x + y)