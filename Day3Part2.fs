namespace Advent2024
open System

    module Day3Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input3.txt" |> List.ofSeq 

        let stringContainsOnlyDigits (s:string) = s |> Seq.forall Char.IsDigit

        let result = 
            lines |> 
            List.reduce (fun x y -> x + y) |>
            (fun s -> s.Split "do()") |>
            Array.map (fun s -> s.Split "don't()") |>
            Array.map Array.head |>
            Array.map (fun s -> s.Split "mul(") |>
            Array.concat |>
            Array.map (fun s -> s.Split ")") |>
            Array.map Array.head |>
            Array.map (fun s -> s.Split ",") |>
            Array.filter (fun list -> list.Length = 2) |>
            Array.map (Array.filter (fun elem -> stringContainsOnlyDigits elem)) |>
            Array.filter (fun list -> list.Length = 2) |>
            Array.map (Array.filter (fun elem -> int elem >= 0 && int elem <= 999)) |>
            Array.filter (fun list -> list.Length = 2) |>
            Array.map (Array.map int) |>
            Array.map (Array.reduce (fun x y -> x * y)) |>
            Array.reduce (fun x y -> x + y)