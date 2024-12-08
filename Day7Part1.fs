namespace Advent2024

    module Day7Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input7.txt"

        let lines = readLines filePath |> Array.ofSeq 

        type Calibration = { Target: int64; Terms: int64 seq }

        let terms_from_string (s: string) = 
            (s.Split ": ")[1] |>
            (fun s -> s.Split ' ') |>
            Seq.map int64

        let calibration_from_string (s: string) =
            { Target = int64 ((s.Split ": ")[0]); Terms = terms_from_string s}

        let get_new_calibration_using_operator (c: Calibration) (op: int64 -> int64 -> int64) =
            let terms_except_last = Seq.take (Seq.length c.Terms - 1) c.Terms
            let new_target = op c.Target (Seq.last c.Terms)
            { Target = new_target; Terms = terms_except_last }

        let rec is_valid (c: Calibration) =
            if (Seq.length c.Terms) = 1 then
                Seq.head c.Terms = c.Target
            else
                let new_calibration_using_plus = get_new_calibration_using_operator c (-)
                let new_calibration_using_times = get_new_calibration_using_operator c (/)

                let times_is_possible = (c.Target % (Seq.last c.Terms)) = 0
                (times_is_possible && is_valid new_calibration_using_times) || is_valid new_calibration_using_plus
                

        let result = 
            lines |>
            Seq.map calibration_from_string |>
            Seq.filter is_valid |>
            Seq.map (fun c -> c.Target) |>
            Seq.reduce (+)
            