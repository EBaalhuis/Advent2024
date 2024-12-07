namespace Advent2024
    module Day2Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input2.txt" |> List.ofSeq 
        let split (l: string) = l.Split ' '

        let linesAsIntLists = lines |> Seq.map split |> Seq.map (Seq.map int)

        let withoutOneElement (l: int seq) = 
            let last = Seq.length l - 1
            seq { 0 .. last } |> Seq.map (fun x -> Seq.removeAt x l)
        let x = Seq.map withoutOneElement linesAsIntLists

        let isIncreasing (l: int seq) = Seq.map2 (fun x y -> y > x) (Seq.rev (Seq.tail (Seq.rev l))) (Seq.tail l) |> Seq.forall id
        let isDecreasing (l: int seq) = Seq.map2 (fun x y -> y < x) (Seq.rev (Seq.tail (Seq.rev l))) (Seq.tail l) |> Seq.forall id
        let isDifferenceOkay (l: int seq) = Seq.map2 (fun x y -> abs(y-x) >= 1 && abs(y-x) <= 3) (Seq.rev (Seq.tail (Seq.rev l))) (Seq.tail l) |> Seq.forall id

        let increasing = Seq.map (fun l -> Seq.map isIncreasing l) x
        let descreasing = Seq.map (fun l -> Seq.map isDecreasing l) x
        let differenceOkay = Seq.map (fun l -> Seq.map isDifferenceOkay l) x


        let increasingOrDecreasing = Seq.map2 (fun a b -> Seq.map2 (fun x y -> x || y) a b) increasing descreasing

        let safe = Seq.map2 (fun a b -> Seq.map2 (fun x y -> x && y) a b) increasingOrDecreasing differenceOkay

        let result = safe |> Seq.map (Seq.reduce (fun x y -> x || y)) |> Seq.filter id |> Seq.length