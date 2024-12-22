namespace Advent2024

open System.Numerics
open System.Linq
open System

    module Day17Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input17.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        let a = (lines[0].Split " ")[2] |> int64 |> bigint
        let b = (lines[1].Split " ")[2] |> int64 |> bigint
        let c = (lines[2].Split " ")[2] |> int64 |> bigint
        let program = (lines[4].Split " ")[1] |> (fun s -> s.Split ",") |> Seq.map int |> Array.ofSeq

        let getCombo (operand: int) (a: bigint) (b: bigint) (c: bigint) =
            match operand with
            | 4 -> a
            | 5 -> b
            | 6 -> c
            | n -> bigint n

        let rec doOperation (a: bigint) (b: bigint) (c: bigint) (instrPointer: int) (output: string) =
            if instrPointer >= Array.length program then output else
            let opCode = program[instrPointer]
            let operand = program[instrPointer+1]
            let combo = getCombo operand a b c
            if opCode = 0 then
                doOperation (a >>> (int combo)) b c (instrPointer + 2) output 
            else if opCode = 1 then
                doOperation a (b^^^(bigint operand)) c (instrPointer + 2) output 
            else if opCode = 2 then
                doOperation a (combo % (bigint 8)) c (instrPointer + 2) output 
            else if opCode = 3 then
                let destination = if a = (bigint 0) then (instrPointer+2) else operand
                doOperation a b c destination output 
            else if opCode = 4 then
                doOperation a (b^^^c) c (instrPointer + 2) output 
            else if opCode = 5 then
                let newOutput = if output = "" then (combo % (bigint 8)).ToString() else output + "," + (combo % (bigint 8)).ToString()
                doOperation a b c (instrPointer + 2) newOutput 
            else if opCode = 6 then
                doOperation a (a >>> (int combo)) c (instrPointer + 2) output 
            else if opCode = 7 then
                doOperation a b (a >>> (int combo)) (instrPointer + 2) output 
            else "error"

        let f (x: bigint) (a: char) : bigint = 
            (BigInteger.Parse (a.ToString())) + (x * (bigint 2))
            
        let bigIntegerFromBinary (st: string) =
            st.Aggregate(BigInteger.Zero, f)

        let getThreeDigitBinary (x: int) =
            match x with
            | 0 -> "000"
            | 1 -> "001"
            | 2 -> "010"
            | 3 -> "011"
            | 4 -> "100"
            | 5 -> "101"
            | 6 -> "110"
            | _ -> "111"

        // Program: 2,4,1,3,7,5,4,0,1,3,0,3,5,5,3,0
        let prefix = "11000100101"

        let rec getResult (prefix: string) (skips: int) =
            if skips < 0 then (Seq.singleton prefix) else
            let target = 
                (lines[4].Split " ")[1] |> 
                Seq.skip skips |> 
                String.Concat
            let options = 
                [0 .. 7] |>
                Seq.map (fun x -> (x,(bigIntegerFromBinary (prefix + (getThreeDigitBinary x))))) |>
                Seq.map (fun pair -> (getThreeDigitBinary (fst pair), doOperation (snd pair) b c 0 "")) |>
                Seq.filter (fun pair -> snd pair = target) |>
                Seq.map fst
            if Seq.length options = 0 then Seq.empty else
            //let res = doOperation (bigIntegerFromBinary (prefix + newDigits)) b c 0 ""
            options |>
            Seq.map (fun newDigits -> getResult (prefix + newDigits) (skips - 2)) |>
            Seq.concat

        let result = 
            getResult "" 30 |>
            Seq.sort |>
            Seq.head |>
            bigIntegerFromBinary
            