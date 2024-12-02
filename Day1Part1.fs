﻿namespace Advent2024
    module Day1Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input1.txt"

        let takeLeft (line: string) = line.Split ' ' |> Array.head
        let takeRight (line: string) = line.Split ' ' |> Array.last

        let a = List.ofSeq(lines) |> List.map(takeLeft) |> List.sort |> List.map int
        let b = List.ofSeq(lines) |> List.map(takeRight) |> List.sort |> List.map int

        let diffs = List.map2 (fun x y -> abs(x - y)) a b

        let result = List.reduce (fun x y -> x + y) diffs