namespace Advent2024

    module Day25Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "example25.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        
        let result = 
            -1