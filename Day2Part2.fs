namespace Advent2024
    module Day2Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input2.txt" |> List.ofSeq 
        let split (l: string) = l.Split ' '

        let linesAsIntLists = lines |> List.map split |> List.map List.ofArray |> List.map (List.map int)

        let withoutOneElement (l: int list) = seq { 0 .. l.Length-1 } |> List.ofSeq |> List.map (fun x -> List.removeAt x l)
        let x = List.map withoutOneElement linesAsIntLists

        let isIncreasing (l: int list) = List.map2 (fun x y -> y > x) (List.rev (List.tail (List.rev l))) l.Tail |> List.forall id
        let isDecreasing (l: int list) = List.map2 (fun x y -> y < x) (List.rev (List.tail (List.rev l))) l.Tail |> List.forall id
        let isDifferenceOkay (l: int list) = List.map2 (fun x y -> abs(y-x) >= 1 && abs(y-x) <= 3) (List.rev (List.tail (List.rev l))) l.Tail |> List.forall id

        let increasing = List.map (fun l -> List.map isIncreasing l) x
        let descreasing = List.map (fun l -> List.map isDecreasing l) x
        let differenceOkay = List.map (fun l -> List.map isDifferenceOkay l) x


        let increasingOrDecreasing = List.map2 (fun a b -> List.map2 (fun x y -> x || y) a b) increasing descreasing

        let safe = List.map2 (fun a b -> List.map2 (fun x y -> x && y) a b) increasingOrDecreasing differenceOkay

        let result = safe |> List.map (List.reduce (fun x y -> x || y)) |> List.filter id |> List.length