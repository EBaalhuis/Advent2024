namespace Advent2024

    module Day23Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "example23.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        
        let result = 
            -1