﻿namespace Advent2024

    module Day20Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "example20.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        
        let result = 
            -1