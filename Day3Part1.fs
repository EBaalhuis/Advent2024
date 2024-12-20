﻿namespace Advent2024
open System

    module Day3Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input3.txt" |> List.ofSeq 

        let getStringsBetweenMul (s:string) = 
            s |>
            (fun s -> s.Split "mul(") |>
            Array.map (fun s -> s.Split ")") |>
            Array.map Array.head

        let stringContainsOnlyDigits (s:string) = s |> Seq.forall Char.IsDigit

        let getTwoSetsOfDigitsAroundComma (s:string) =
            s |>
            (fun s -> s.Split ",") |>
            (fun arr -> if arr.Length = 2 then arr else Array.empty) |>
            Array.filter (fun elem -> stringContainsOnlyDigits elem) |>
            (fun arr -> if arr.Length = 2 then arr else Array.empty) |>
            Array.map int

        let whereBothNumbersWithinLimits (arr: int seq) =
            arr |>
            (fun arr -> if Seq.forall (fun elem -> elem >= 0 && elem <= 999) arr then arr else Seq.empty)


        let result = 
            lines |> 
            Seq.reduce (fun x y -> x + y) |>
            getStringsBetweenMul |>
            Seq.map getTwoSetsOfDigitsAroundComma |>
            Seq.map whereBothNumbersWithinLimits |>
            Seq.map (fun sq -> if (Seq.length sq) = 0 then 0 else Seq.reduce (fun x y -> x * y) sq) |>
            Seq.reduce (fun x y -> x + y)