namespace Advent2024

    module Day21Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "example21.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        
        let result = 
            -1