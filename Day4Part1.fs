namespace Advent2024
open System

    module Day4Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input4.txt" |> List.ofSeq 

        let count_occurrences_of_word (lines:string seq) (word: string) =
            lines |>
            Seq.map (fun s -> s.Split word) |>
            Seq.map (fun l -> l.Length - 1) |>
            Seq.reduce (+)

        let count_occurrences_of_word_and_its_reverse (lines: string seq) (word: string) =
            count_occurrences_of_word lines word + count_occurrences_of_word lines (String.Concat(Seq.rev word))

        let transpose (lines:string seq) =
            [0 .. (List.ofSeq lines).Head.Length-1] |>
            Seq.map (fun x -> (Seq.map (Seq.skip x) lines)) |>
            Seq.map (Seq.map Seq.head) |>
            Seq.map String.Concat

        let prepend_n_spaces (n: int) (s: string) =
            s |>
            Seq.append (Seq.init n (fun _ -> ' ')) |>
            String.Concat

        let append_n_spaces (n: int) (s: string) =
            s |>
            Seq.rev |>
            Seq.append (Seq.init n (fun _ -> ' ')) |>
            Seq.rev |>
            String.Concat

        let diagonalise_bottom_left_top_right (lines:string list) =
            [0 .. lines.Length - 1] |>
            Seq.map (fun x -> 
                (Array.ofSeq lines)[x] |> 
                prepend_n_spaces x |>
                append_n_spaces (lines.Length - x)) |>
            Seq.map String.Concat |>
            transpose

        let diagonalise_top_left_bottom_right (lines:string list) =
            [0 .. lines.Length - 1] |>
            Seq.map (fun x -> 
                (Array.ofSeq lines)[x] |> 
                prepend_n_spaces (lines.Length - x) |>
                append_n_spaces x) |>
            Seq.map String.Concat |>
            transpose

        let result = 
            count_occurrences_of_word_and_its_reverse lines "XMAS" +
            count_occurrences_of_word_and_its_reverse (transpose lines) "XMAS" +
            count_occurrences_of_word_and_its_reverse (diagonalise_bottom_left_top_right lines) "XMAS" +
            count_occurrences_of_word_and_its_reverse (diagonalise_top_left_bottom_right lines) "XMAS"