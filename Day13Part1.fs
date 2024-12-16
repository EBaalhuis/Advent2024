namespace Advent2024

open System

    module Day13Part1 =
        let readText filePath = System.IO.File.ReadAllText(filePath);
        let filePath = "input13.txt"

        let text = readText filePath 
        
        type Machine = {a_x: int; a_y: int; b_x: int; b_y: int; prize_x: int; prize_y: int}

        let machines =
            text.Split "Button A: X+" |>
            Seq.tail |>
            Seq.map (fun s -> s.Split " ") |>
            Seq.map (Seq.map (Seq.filter (fun c -> Char.IsDigit c))) |>
            Seq.map (Seq.map String.Concat) |>
            Seq.map (Seq.filter (fun s -> not (s = ""))) |>
            Seq.map (Seq.map int) |>
            Seq.map Array.ofSeq |>
            Seq.map (fun arr -> { a_x = arr[0]; a_y=arr[1]; b_x=arr[2]; b_y=arr[3]; prize_x=arr[4]; prize_y=arr[5]; })


        let options = Seq.allPairs [0 .. 100] [0 .. 100]

        let optionWorksForMachine (m: Machine) (option: (int*int)) =
            fst option * m.a_x + snd option * m.b_x = m.prize_x && fst option * m.a_y + snd option * m.b_y = m.prize_y

        let costOfOption (option: (int*int)) =
            3 * fst option + snd option

        let workingCosts =
            machines |>
            Seq.map (fun m -> options |> Seq.filter (optionWorksForMachine m) |> Seq.map costOfOption) |>
            Seq.filter (fun s -> not (Seq.length s = 0))
            
        let result = workingCosts |> Seq.map Seq.min |> Seq.reduce (+)

        