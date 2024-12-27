namespace Advent2024

    module Day19Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "example19.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        
        let result = 
            -1