namespace Advent2024
    module Day2Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input2.txt" |> List.ofSeq 
        let split (l: string) = l.Split ' '

        let linesAsIntLists = lines |> List.map split |> List.map List.ofArray |> List.map (List.map int)

        let isIncreasing (l: int list) = List.map2 (fun x y -> y > x) (List.rev (List.tail (List.rev l))) l.Tail |> List.forall id
        let isDecreasing (l: int list) = List.map2 (fun x y -> y < x) (List.rev (List.tail (List.rev l))) l.Tail |> List.forall id

        let increasing = List.map isIncreasing linesAsIntLists
        let descreasing = List.map isDecreasing linesAsIntLists
        let increasingOrDecreasing = List.map2 (fun x y -> x || y) increasing descreasing

        let isDifferenceOkay (l: int list) = List.map2 (fun x y -> abs(y-x) >= 1 && abs(y-x) <= 3) (List.rev (List.tail (List.rev l))) l.Tail |> List.forall id
        let differenceOkay = List.map isDifferenceOkay linesAsIntLists

        let safe = List.map2 (fun x y -> x && y) increasingOrDecreasing differenceOkay

        let result = safe |> List.filter id |> List.length