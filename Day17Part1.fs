namespace Advent2024

    module Day17Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input17.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        let a = (lines[0].Split " ")[2] |> int64
        let b = (lines[1].Split " ")[2] |> int64
        let c = (lines[2].Split " ")[2] |> int64
        let program = (lines[4].Split " ")[1] |> (fun s -> s.Split ",") |> Seq.map int |> Array.ofSeq

        let getCombo (operant: int) (a: int64) (b: int64) (c: int64) =
            match operant with
            | 4 -> a
            | 5 -> b
            | 6 -> c
            | n -> n

        let rec doOperation (a: int64) (b: int64) (c: int64) (instrPointer: int) (output: string) =
            if instrPointer >= Array.length program then output else
            let opCode = program[instrPointer]
            let operand = program[instrPointer+1]
            let combo = getCombo operand a b c
            if opCode = 0 then
                doOperation (a >>> (int combo)) b c (instrPointer + 2) output 
            else if opCode = 1 then
                doOperation a (b^^^operand) c (instrPointer + 2) output 
            else if opCode = 2 then
                doOperation a (combo % 8L) c (instrPointer + 2) output 
            else if opCode = 3 then
                let destination = if a = 0 then (instrPointer+2) else operand
                doOperation a b c destination output 
            else if opCode = 4 then
                doOperation a (b^^^c) c (instrPointer + 2) output 
            else if opCode = 5 then
                let newOutput = if output = "" then (combo % 8L).ToString() else output + "," + (combo % 8L).ToString()
                doOperation a b c (instrPointer + 2) newOutput 
            else if opCode = 6 then
                doOperation a (a >>> (int combo)) c (instrPointer + 2) output 
            else if opCode = 7 then
                doOperation a b (a >>> (int combo)) (instrPointer + 2) output 
            else "error"


        let result = doOperation a b c 0 ""
        
