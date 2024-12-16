namespace Advent2024

open System

    module Day13Part2 =
        let readText filePath = System.IO.File.ReadAllText(filePath);
        let filePath = "input13.txt"

        let text = readText filePath 
        
        type Machine = {a_x: int64; a_y: int64; b_x: int64; b_y: int64; prize_x: int64; prize_y: int64}

        let machines =
            text.Split "Button A: X+" |>
            Seq.tail |>
            Seq.map (fun s -> s.Split " ") |>
            Seq.map (Seq.map (Seq.filter (fun c -> Char.IsDigit c))) |>
            Seq.map (Seq.map String.Concat) |>
            Seq.map (Seq.filter (fun s -> not (s = ""))) |>
            Seq.map (Seq.map int64) |>
            Seq.map Array.ofSeq |>
            Seq.map (fun arr -> { a_x = arr[0]; a_y=arr[1]; b_x=arr[2]; b_y=arr[3]; prize_x=arr[4]+10000000000000L; prize_y=arr[5]+10000000000000L; })

        let getSolution (m: Machine) =
            let len_x = (double)m.a_x - ((double)m.a_y * (double)m.b_x / (double)m.b_y)
            let unit_x = (1. / len_x, -1. * (double)m.a_y / (double)m.b_y / len_x)
            let len_y = (double)m.a_y - ((double)m.a_x * (double)m.b_y / (double)m.b_x)
            let unit_y = (1. / len_y, -1. * (double)m.a_x / (double)m.b_x / len_y)
            ((double)m.prize_x * (fst unit_x) + (double)m.prize_y * (fst unit_y), (double)m.prize_x * (snd unit_x) + (double)m.prize_y * (snd unit_y))
        

        let costOfOption (option: (double*double)) =
            int64 (round (3. * fst option + snd option))
            
        let solutions = 
            machines |>
            Seq.map (fun m -> (m, getSolution m)) |>
            Seq.filter (fun x -> (fst x).prize_x = (int64 (round (fst (snd x))))*(fst x).a_x + (int64 (round (snd (snd x))))*(fst x).b_x && 
                                 (fst x).prize_y = (int64 (round (fst (snd x))))*(fst x).a_y + (int64 (round (snd (snd x))))*(fst x).b_y)

        let result = 
            solutions |>
            Seq.map (fun x -> costOfOption (snd x)) |>
            Seq.reduce (+)

        