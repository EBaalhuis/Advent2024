namespace Advent2024

    module Day9Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input9.txt"

        let lines = readLines filePath |> Array.ofSeq 
        let numbers = lines[0] |> Seq.map (fun c -> c.ToString()) |> Seq.map int |> Array.ofSeq
                    

        let rec checksum (position: int64) (frontFileId: int64) (endFileId: int64) (takeFromEnd: bool) (total: int64) (frontCur: int) (endCur: int) (frontIndex: int) (endIndex: int) =
            if endIndex < frontIndex || (endIndex = frontIndex && frontCur + endCur >= numbers[frontIndex]) then total
            else if numbers[frontIndex] = frontCur then
                let newFrontFileId = if takeFromEnd then frontFileId + 1L else frontFileId
                checksum position newFrontFileId endFileId (not takeFromEnd) total 0 endCur (frontIndex+1) endIndex
            else if not takeFromEnd then
                //let newNumbers = Seq.append (Seq.singleton((Seq.head numbers) - 1)) (Seq.tail numbers)
                let newTotal = total + position * frontFileId
                checksum (position + 1L) frontFileId endFileId takeFromEnd newTotal (frontCur + 1) endCur frontIndex endIndex
            else 
                if numbers[endIndex] = endCur then
                    checksum position frontFileId (endFileId - 1L) takeFromEnd total frontCur 0 frontIndex (endIndex-2)
                else
                    let newTotal = total + position * endFileId
                    checksum (position + 1L) frontFileId endFileId takeFromEnd newTotal (frontCur+1) (endCur+1) frontIndex endIndex

        let result = checksum 0 0 (int64 (Seq.length numbers) / 2L) false 0 0 0 0 (Seq.length numbers - 1)
            