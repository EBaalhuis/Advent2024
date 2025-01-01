namespace Advent2024

open System.Collections.Generic

    module Day21Part1 =
        let readLines filePath = System.IO.File.ReadLines(filePath);
        let filePath = "input21.txt"

        let lines = readLines filePath |> Array.ofSeq 
        
        type DirectionKey = 
            | Up 
            | Down
            | Left
            | Right
            | DA

        type NumKey =
            | N1
            | N2
            | N3
            | N4
            | N5
            | N6
            | N7
            | N8
            | N9
            | N0
            | NA

        type State = { Robot1Key: DirectionKey; Robot2Key: DirectionKey; Robot3Key: NumKey }

        let getNeighborDirectionKeys (k: DirectionKey) =
            match k with
            | Up -> [(Down, Down); (DA, Right)]
            | Down -> [(Left, Left); (Up, Up); (Right, Right)]
            | Left -> [(Down, Right)]
            | Right -> [(Down, Left); (DA, Up)]
            | DA -> [(Up, Left); (Right, Down)]

        let getMoveDir (d: DirectionKey) (pressed: DirectionKey) =
            match pressed with 
            | Up ->
                match d with
                | Up -> []
                | Down -> [Up]
                | Left -> []
                | Right -> [DA]
                | DA -> []
            | Down->
                match d with
                | Up -> [Down]
                | Down -> []
                | Left -> []
                | Right -> []
                | DA -> [Right]
            | Left->
                match d with
                | Up -> []
                | Down -> [Left]
                | Left -> []
                | Right -> [Down]
                | DA -> [Up]
            | Right->
                match d with
                | Up -> [DA] 
                | Down -> [Right]
                | Left -> [Down]
                | Right -> []
                | DA -> []
            | DA->
                [d]

        let getMoveNum (n: NumKey) (pressed: DirectionKey) =
            match pressed with 
            | Up ->
                match n with
                | N1 -> [N4]
                | N2 -> [N5]
                | N3 -> [N6]
                | N4 -> [N7]
                | N5 -> [N8]
                | N6 -> [N9]
                | N7 -> []
                | N8 -> []
                | N9 -> []
                | N0 -> [N2]
                | NA -> [N3]
            | Down ->
                match n with
                | N1 -> []
                | N2 -> [N0]
                | N3 -> [NA]
                | N4 -> [N1]
                | N5 -> [N2]
                | N6 -> [N3]
                | N7 -> [N4]
                | N8 -> [N5]
                | N9 -> [N6]
                | N0 -> []
                | NA -> []
            | Left ->
                match n with
                | N1 -> []
                | N2 -> [N1]
                | N3 -> [N2]
                | N4 -> []
                | N5 -> [N4]
                | N6 -> [N5]
                | N7 -> []
                | N8 -> [N7]
                | N9 -> [N8]
                | N0 -> []
                | NA -> [N0] 
            | Right ->
                match n with
                | N1 -> [N2]
                | N2 -> [N3]
                | N3 -> []
                | N4 -> [N5]
                | N5 -> [N6]
                | N6 -> []
                | N7 -> [N8]
                | N8 -> [N9]
                | N9 -> []
                | N0 -> [NA]
                | NA -> []
            | DA ->
                [n]

        let getNeighborStates (s: State) =
            let reachedWithArrow =
                getNeighborDirectionKeys s.Robot1Key |>
                Seq.map (fun pair -> ({ Robot1Key = fst pair; Robot2Key = s.Robot2Key; Robot3Key = s.Robot3Key }, snd pair))
            let reachedWithActivate =
                let newRobot2Keys = getMoveDir s.Robot2Key s.Robot1Key
                let newRobot3Keys = 
                    if s.Robot1Key = DA then getMoveNum s.Robot3Key s.Robot2Key
                    else [s.Robot3Key]
                Seq.allPairs newRobot2Keys newRobot3Keys |>
                Seq.map (fun pair -> { Robot1Key = s.Robot1Key; Robot2Key = (fst pair); Robot3Key = (snd pair) }) |>
                Seq.map (fun s -> (s, DA))
            Seq.append reachedWithArrow reachedWithActivate

        let getString (d: DirectionKey) =
            match d with
            | Up -> "^"
            | Down -> "v"
            | Left -> "<"
            | Right -> ">"
            | DA -> "A"

        let stateMatchesDigit (s: State) (digit: char) =
            let digitIsOk =
                match digit with
                | '0' -> s.Robot3Key = N0
                | '1' -> s.Robot3Key = N1
                | '2' -> s.Robot3Key = N2
                | '3' -> s.Robot3Key = N3
                | '4' -> s.Robot3Key = N4
                | '5' -> s.Robot3Key = N5
                | '6' -> s.Robot3Key = N6
                | '7' -> s.Robot3Key = N7
                | '8' -> s.Robot3Key = N8
                | '9' -> s.Robot3Key = N9
                | _ -> s.Robot3Key = NA
            digitIsOk && s.Robot1Key = DA && s.Robot2Key = DA
        
        let getScore (line: string) =
            let rec update (s: State) (newValue: int64) (pathSoFar: string) (dist: Dictionary<State, int64>) (path: Dictionary<State, string>) =
                if not (dist.ContainsKey s) || dist[s] > newValue then
                    dist[s] <- newValue
                    path[s] <- pathSoFar
                    for newState in getNeighborStates s do
                        update (fst newState) (newValue + 1L) (pathSoFar + getString (snd newState)) dist path

            let findPathsToDigit (index: int) (oldDicts: Dictionary<State, int64> * Dictionary<State, string>) =
                let oldDists = fst oldDicts
                let oldPaths = snd oldDicts
                let newDists = new Dictionary<State, int64>()
                let newPaths = new Dictionary<State, string>()
                let statesForDigit = 
                    oldDists.Keys |>
                    Seq.filter (fun a -> stateMatchesDigit a line[index])
                for s in statesForDigit do
                    update s (oldDists[s]+1L) (oldPaths[s]+"A") newDists newPaths
                (newDists, newPaths)

            let distanceFromStart = new Dictionary<State, int64>()
            let shortestPath = new Dictionary<State, string>()
            let newDists = new Dictionary<State, int64>()
            let newPaths = new Dictionary<State, string>()

            update {Robot1Key = DA; Robot2Key = DA; Robot3Key = NA} 0L "" distanceFromStart shortestPath

            let repeat n =
                Seq.init n (fun i u -> findPathsToDigit i u) |>
                Seq.reduce (>>)

            let (_, finalPaths) = 
                repeat 3 (distanceFromStart, shortestPath)

            let pathLength = 
                finalPaths.Keys |>
                Seq.filter (fun a -> stateMatchesDigit a line[3]) |>
                Seq.map (fun a -> finalPaths[a]) |>
                Seq.map (fun s -> s.Length) |>
                Seq.min |>
                (fun x -> x + 1)

            let multiplier = line.Substring(0,3) |> int64
            (int64 pathLength) * multiplier

        let test =
            lines |>
            Seq.map getScore

        let result = 
            test |>
            Seq.reduce (+)