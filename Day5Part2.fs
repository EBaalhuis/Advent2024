namespace Advent2024
open System

    module Day5Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);

        let lines = readLines "input5.txt" |> List.ofSeq 

        let rules = 
            lines |>
            Seq.filter (fun (s: string) -> s.Contains '|') |>
            Seq.map (fun s -> s.Split '|') |>
            Seq.map (fun strings -> (strings[0], strings[1]))

        let filter_rules_by_second (rules: (string * string) seq) (filter_string: string) =
            Seq.filter (fun rule -> snd rule = filter_string) rules |>
            Seq.map fst

        let rules_map =
            rules |>
            Seq.map snd |>
            Seq.distinct |>
            Seq.map (fun x -> (x, Set.ofSeq (filter_rules_by_second rules x))) |>
            Map.ofSeq

        let updates = 
            lines |>
            Seq.filter (fun (s: string) -> not (s.Contains '|') && not (s = "")) |>
            Seq.map (fun s -> s.Split ',') 

        let rec update_is_valid_rec (update: string seq) (forbidden: string Set) =
            if Seq.isEmpty update then
                true
            else if Set.contains (Seq.head update) forbidden then
                false
            else
                let add_to_forbidden = 
                    if rules_map.ContainsKey (Seq.head update) then rules_map[(Seq.head update)]
                    else Set.empty
                update_is_valid_rec (Seq.skip 1 update) (Set.union add_to_forbidden forbidden)

        let update_is_valid (update: string seq) =
            update_is_valid_rec update Set.empty

        let rec reorder (update: string seq) =
            if Seq.isEmpty update then
                Seq.empty
            else 
                let forbidden = 
                    update |>
                    Seq.map (fun x -> if (rules_map.ContainsKey x) then rules_map[x] else Set.empty) |>
                    Set.unionMany
                let last_page = 
                    update |>
                    Seq.filter (fun page -> not (Set.contains page forbidden)) |>
                    Seq.head
                let update_without_last_page =
                    update |>
                    Seq.filter (fun page -> not (page = last_page))
                Seq.append (reorder update_without_last_page) (Seq.singleton last_page)

        let get_score_with_reorder (update: string seq) =
            update |>
            reorder |>
            (fun x -> (Array.ofSeq x)[(Array.ofSeq x).Length / 2]) |>
            int

        let result = 
            updates |>
            Seq.filter (fun update -> not (update_is_valid update)) |>
            Seq.map get_score_with_reorder |>
            Seq.reduce (+)
