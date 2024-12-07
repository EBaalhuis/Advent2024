namespace Advent2024
    module Day2Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input2.txt" |> List.ofSeq 
        let split (l: string) = l.Split ' '

        let linesAsIntLists = lines |> Seq.map split |> Seq.map (Seq.map int)

        let isIncreasing (l: int seq) = Seq.map2 (fun x y -> y > x) (Seq.rev (Seq.tail (Seq.rev l))) (Seq.tail l) |> Seq.forall id
        let isDecreasing (l: int seq) = Seq.map2 (fun x y -> y < x) (Seq.rev (Seq.tail (Seq.rev l))) (Seq.tail l) |> Seq.forall id

        let increasing = Seq.map isIncreasing linesAsIntLists
        let descreasing = Seq.map isDecreasing linesAsIntLists
        let increasingOrDecreasing = Seq.map2 (fun x y -> x || y) increasing descreasing

        let isDifferenceOkay (l: int seq) = Seq.map2 (fun x y -> abs(y-x) >= 1 && abs(y-x) <= 3) (Seq.rev (Seq.tail (Seq.rev l))) (Seq.tail l) |> Seq.forall id
        let differenceOkay = Seq.map isDifferenceOkay linesAsIntLists

        let safe = Seq.map2 (fun x y -> x && y) increasingOrDecreasing differenceOkay

        let result = safe |> Seq.filter id |> Seq.length