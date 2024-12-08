namespace Advent2024

    module Day7Part2 =
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

        let concat_operator (x: int64) (y: int64) : int64 =
            let nr_digits_to_remove = y.ToString().Length
            let x_string = x.ToString()
            let last_digits_removed = x_string.Substring(0, x_string.Length - nr_digits_to_remove)
            if last_digits_removed = "" then 0 else int64 last_digits_removed

        let get_new_calibration_using_operator (c: Calibration) (op: int64 -> int64 -> int64) =
            let terms_except_last = Seq.take (Seq.length c.Terms - 1) c.Terms
            let new_target = op c.Target (Seq.last c.Terms)
            { Target = new_target; Terms = terms_except_last }

        let concat_is_possible (c: Calibration) =
            let target_string = c.Target.ToString()
            let final_term_string = (Seq.last c.Terms).ToString()
            target_string.Substring(target_string.Length - final_term_string.Length) = final_term_string

        let rec is_valid (c: Calibration) =
            if (Seq.length c.Terms) = 1 then
                (Seq.head c.Terms) = c.Target
            else if c.Target < (Seq.last c.Terms) then
                false
            else
                let new_calibration_using_concat = get_new_calibration_using_operator c concat_operator
                let new_calibration_using_times = get_new_calibration_using_operator c (/)
                let new_calibration_using_plus = get_new_calibration_using_operator c (-)

                let times_is_possible = (c.Target % (Seq.last c.Terms)) = 0
                
                (concat_is_possible c && is_valid new_calibration_using_concat) ||
                (times_is_possible && is_valid new_calibration_using_times) || 
                is_valid new_calibration_using_plus
                

        let result = 
            lines |>
            Seq.map calibration_from_string |>
            Seq.filter is_valid |>
            Seq.map (fun c -> c.Target) |>
            Seq.reduce (+)
            