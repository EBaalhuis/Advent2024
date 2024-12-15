namespace Advent2024

open System.Linq

    module Day9Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input9.txt"

        let lines = readLines filePath |> Array.ofSeq 
        let numbers = lines[0] |> Seq.map (fun c -> c.ToString()) |> Seq.map int |> Array.ofSeq
                    
        let expanded =
            let f index elem = 
                let nr = if index % 2 = 0 then index / 2 else -1
                (elem, nr)
            numbers |>
            Array.mapi f 

        let rec trySwap (expanded: (int*int) array) (fileId: int) =
            if fileId = 0 then expanded 
            else 
                let findOldIndex index elem =
                    if snd elem = fileId then index else -1
                let oldIndex = Array.mapi findOldIndex expanded |> Seq.filter (fun x -> x >= 0) |> Seq.head

                let len = expanded |> Seq.filter (fun x -> snd x = fileId) |> Seq.head |> fst |> int
                let f index elem =
                    if fst elem >= len && snd elem = -1 then index else -1
                let newIndexOptions = expanded |> Array.mapi f |> Seq.map int |> Seq.filter (fun x -> x >= 0) 
                let newIndex = if Seq.length newIndexOptions > 0 then Seq.head newIndexOptions else -1
                if newIndex = -1 || newIndex >= oldIndex then trySwap expanded (fileId - 1)
                else
                    let start = Seq.take newIndex expanded
                    let insert1 = expanded |> Seq.filter (fun x -> snd x = fileId) |> Seq.head
                    let insert2 = ((expanded[newIndex] |> fst) - len, -1)
                    let middle = Seq.skip (newIndex + 1) expanded |> Seq.take (oldIndex - newIndex - 1)
                    let insert3 = (len, -1)
                    let end' = Seq.skip (oldIndex + 1) expanded |> Seq.filter (fun x -> not ((snd x) = fileId))
                    let part1 = Seq.append start (Seq.append (Seq.singleton insert1) (Seq.append (Seq.singleton insert2) middle))
                    let part2 = Seq.append (Seq.singleton insert3) end'
                    let newExpanded = Seq.append part1 part2 |> Array.ofSeq
                    trySwap newExpanded (fileId - 1)

        let initialFileId = Seq.length expanded / 2
        let swapped = trySwap expanded initialFileId

        let a = swapped |> Seq.map (fun x -> Enumerable.Repeat ((snd x), (fst x))) |> Seq.concat |> Seq.map (fun x -> if x = -1 then 0 else x)
        let result = a |> Array.ofSeq |> Array.mapi (fun index elem -> index * elem) |> Array.map int64 |> Array.reduce (+)
                
