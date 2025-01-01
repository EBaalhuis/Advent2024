namespace Advent2024

open System.Collections.Generic

    module Day22Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input22.txt"

        let lines = readLines filePath |> Array.ofSeq 
        let starts = lines |> Array.map int64
        
        let m = 16777216L

        let getNextNumber (s: int64) =
            let a = s * 64L
            let b = (s ^^^ a) % m
            let c = b / 32L
            let d = (b ^^^ c) % m
            let e = d * 2048L
            let f = (d ^^^ e) % m
            f

        let repeat n =
            if n = 0 then id else
            Seq.init n (fun _ u -> getNextNumber u) |>
            Seq.reduce (>>)

        let rec getSecrets (current: int64) (count: int) =
            if count = 0 then Seq.empty else
            Seq.append (Seq.singleton current) (getSecrets (getNextNumber current) (count - 1))

        let getDiffs (prices: int64 seq) =
            Seq.pairwise prices |>
            Seq.map (fun pair -> snd pair - fst pair)

        let diffSeqs = 
            Seq.allPairs [-9L .. 9L] [-9L .. 9L] |>
            (fun a -> Seq.allPairs a [-9L .. 9L]) |>
            (fun a -> Seq.allPairs a [-9L .. 9L]) |>
            Seq.map (fun a -> (fst (fst (fst a)), snd (fst (fst a)), snd (fst a), snd a))

        let result = 
            let scoresPerLine = new Dictionary<int*int64*int64*int64*int64, int64>()
            for i in [0 .. lines.Length - 1] do
                let start = starts[i]
                let prices = getSecrets start 2001 |> Seq.map (fun x -> x % 10L) |> Array.ofSeq
                let diffs = getDiffs prices |> Array.ofSeq
                for j in [3 .. Seq.length diffs - 1] do
                    let key = (i, diffs[j-3], diffs[j-2], diffs[j-1], diffs[j])
                    let price = prices[j]
                    if not (scoresPerLine.ContainsKey key) then
                        scoresPerLine[key] <- price
            
            let scoresPerDiffSeq = new Dictionary<int64*int64*int64*int64, int64>()
            for key in scoresPerLine.Keys do
                let _, a, b, c, d = key
                let newKey = (a, b, c, d)
                if not (scoresPerDiffSeq.ContainsKey newKey) then
                    scoresPerDiffSeq[newKey] <- 0L
                scoresPerDiffSeq[newKey] <- scoresPerDiffSeq[newKey] + scoresPerLine[key]

            scoresPerDiffSeq.Values |> Seq.max
            //let maxScore = scoresPerDiffSeq.Values |> Seq.max
            //let filtered = 
            //    scoresPerDiffSeq.Keys |>
            //    Seq.filter (fun k -> scoresPerDiffSeq[k] = maxScore)

            //filtered |> Seq.head |> (fun k -> scoresPerDiffSeq[k])