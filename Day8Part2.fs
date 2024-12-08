namespace Advent2024

    module Day8Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input8.txt"

        let lines = readLines filePath |> Array.ofSeq 

        let max_x = lines[0].Length - 1
        let max_y = lines.Length - 1

        let lines_as_char_arrays = lines |> Array.map Array.ofSeq

        let letters = Seq.append { 'a' .. 'z' } { 'A' .. 'Z' }
        let digits = { '0' .. '9' }

        let getPositionsOfChar (c: char) =
            Seq.allPairs { 0 .. max_x } { 0 .. max_y } |>
            Seq.filter (fun pair -> lines_as_char_arrays[snd pair][fst pair] = c)

        let rec gcd (a: int) (b: int) =
            if b = 0 then a
            else gcd b (a % b)

        let getAntinodes (p1: int * int) (p2: int * int) =
            let delta = (fst p1 - fst p2, snd p1 - snd p2)
            let delta_gcd = gcd (fst delta) (snd delta)
            let delta_divided_by_gcd = (fst delta / delta_gcd, snd delta / delta_gcd)
            let max = if max_x > max_y then max_x else max_y
            [ -max .. max ] |>
            Seq.map (fun x -> (fst p1 + x * (fst delta_divided_by_gcd), snd p1 + x * (snd delta_divided_by_gcd)))

        let result = 
            Seq.append letters digits |>
            Seq.map getPositionsOfChar |>
            Seq.map (fun positions -> Seq.allPairs positions positions) |>
            Seq.map (fun positionPairs -> Seq.filter (fun positionPair -> not (fst positionPair = snd positionPair)) positionPairs) |>
            Seq.map (fun positionPairs -> Seq.map (fun positionPair -> getAntinodes (fst positionPair) (snd positionPair)) positionPairs) |>
            Seq.concat |>
            Seq.concat |>
            Seq.filter (fun antinode -> fst antinode >= 0 && fst antinode <= max_y && snd antinode >= 0 && snd antinode <= max_x) |>
            Seq.distinct |>
            Seq.length
            