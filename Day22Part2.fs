namespace Advent2024

    module Day22Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "example22.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        
        let result = 
            -1