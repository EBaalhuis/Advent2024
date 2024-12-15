namespace Advent2024

open System.Collections.Generic

    module Day11Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input11.txt"

        let lines = readLines filePath |> Array.ofSeq 
        let initialStones = 
            lines[0].Split " " |> 
            Seq.map int64 |> 
            Seq.countBy id |> 
            Seq.map (fun pair -> (fst pair, int64 (snd pair))) |> 
            dict
        
        
        let rec applyRule (d: IDictionary<int64, int64>) (blinks: int) =
            if blinks = 0 then d else
            let newDict = new Dictionary<int64, int64>()
            for key in d.Keys do
                if key = 0L then
                    if newDict.ContainsKey(1L) then
                        newDict[1L] <- newDict[1L] + d[key]
                    else
                        newDict.Add(1L, d[key])
                else if key.ToString().Length % 2 = 0 then
                    let newKey1 = int64 (key.ToString().Substring(0, key.ToString().Length / 2))
                    if newDict.ContainsKey(newKey1) then
                        newDict[newKey1] <- newDict[newKey1] + d[key]
                    else
                        newDict.Add(newKey1, d[key])
                    let newKey2 = int64 (key.ToString().Substring(key.ToString().Length / 2, key.ToString().Length / 2))
                    if newDict.ContainsKey(newKey2) then
                        newDict[newKey2] <- newDict[newKey2] + d[key]
                    else
                        newDict.Add(newKey2, d[key])
                else 
                    let newKey = key * 2024L
                    if newDict.ContainsKey(newKey) then
                        newDict[newKey] <- newDict[newKey] + d[key]
                    else
                        newDict.Add(newKey, d[key])
            applyRule newDict (blinks - 1)

        
        let result = applyRule initialStones 75 |> Seq.map (|KeyValue|) |> Seq.map (fun pair -> snd pair) |> Seq.reduce (+)
