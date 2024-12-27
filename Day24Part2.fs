namespace Advent2024

    module Day24Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "example24.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        
        let result = 
            -1