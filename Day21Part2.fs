namespace Advent2024

    module Day21Part2 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input21.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        let rec transpose = function
            | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
            | _ -> []

        let rec inner u v =
            match u, v with 
            | [x], [y] -> x*y   
            | u'::u, v'::v -> u'*v' + inner u v 
            | _,_ -> 0L

        let multiply xs ys =
            [for row in xs ->
                [for col in transpose ys -> inner row col]]

        let m =
            [
                [1L;0L;0L;0L;0L;0L;1L;0L;0L;0L;0L;0L;1L;0L;0L;0L;0L;0L;1L;0L;0L;0L;0L;0;1L];
                [0L;0L;1L;1L;1L;0L;0L;1L;1L;1L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;1L;1L;0L;0L;0L;1L;0L;0L;0L;0L;1L;1L;0L;0;0L];
                [0L;0L;0L;0L;0L;1L;0L;0L;0L;0L;0L;0L;0L;0L;1L;0L;1L;1L;0L;1L;0L;0L;0L;0;0L];
                [0L;1L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;1L;0L;0L;0L;0L;0L;0L;0L;0L;1L;1;0L];
                [0L;0L;0L;0L;1L;0L;0L;1L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;1L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;1L;1L;0L;0L;0L;0L;1L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;1L;0L;0L;0L;0L;1L;0L;0L;0L;1L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;1L;0L;0L;0L;0L;1L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;1L;0L;0;0L];
                [0L;0L;0L;0L;0L;1L;0L;0L;0L;1L;1L;0L;0L;0L;1L;1L;0L;1L;0L;1L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;1L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;1L;0L;0L;0L;1L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;1L;1L;1L;0L;0L;0L;0L;1L;0L;0L;0L;0L;1L;0L;0L;0L;0L;0L;0L;0L;1L;1L;1;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0;0L];
                [0L;0L;0L;1L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;1;0L]
            ]

        let vector029A = [[0L;1L;1L;1L;1L;2L;1L;0L;0L;0L;1L;0L;2L;0L;0L;1L;0L;0L;0L;0L;0L;1L;0L;0L;0L]]

        let vector964A = [[0L;1L;2L;1L;0L;1L;2L;0L;0L;0L;1L;0L;1L;0L;1L;1L;0L;0L;1L;0L;1L;0L;0L;0L;1L]]
        let vector246A = [[0L;2L;1L;0L;1L;0L;0L;0L;2L;0L;1L;0L;1L;0L;0L;2L;0L;0L;0L;0L;1L;0L;0L;0L;1L]]
        let vector973A = [[0L;1L;1L;1L;1L;1L;2L;0L;0L;0L;2L;0L;1L;0L;0L;1L;0L;0L;1L;0L;0L;0L;1L;0L;1L]]
        let vector682A = [[0L;2L;1L;0L;1L;1L;1L;0L;1L;0L;2L;0L;1L;0L;0L;1L;0L;0L;0L;0L;0L;0L;1L;0L;0L]]
        let vector180A = [[0L;0L;1L;1L;2L;2L;1L;0L;0L;0L;1L;0L;2L;0L;0L;0L;1L;0L;1L;0L;1L;1L;0L;0L;0L]]

        let repeat n =
            Seq.init n (fun _ u -> multiply m u) |>
            Seq.reduce (>>)

        let getScore (v: int64 list list) =
            (repeat 25) (transpose v) |> 
            Seq.concat |>
            Seq.reduce(+)

        let a = 964L * getScore vector964A 
        let b = 246L * getScore vector246A 
        let c = 973L * getScore vector973A 
        let d = 682L * getScore vector682A 
        let e = 180L * getScore vector180A

        let result = 
            964L * getScore vector964A +
            246L * getScore vector246A +
            973L * getScore vector973A +
            682L * getScore vector682A +
            180L * getScore vector180A