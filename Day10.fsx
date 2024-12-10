#load "lib/Helper.fsx"
open Helper

// Day 10: Hoof It
//
// Part 1 - Use a dfs from each 0 to find all the points that end in 9, using a
//          set to ensure uniqueness
// Part 2 - Easier than part 1. We just add up all the ways a dfs will get to 9

let part1 map =
    let rec dfs (map : int [] []) pos prevValue =
        match ArrayOfArrays.tryGet pos map with
        | None -> Set.empty
        | Some 9 when prevValue = 8 -> Set [pos]
        | Some value when value = prevValue + 1 ->
            [ -1,0; 0,1; 1,0; 0,-1 ]
            |> List.map (fun dir -> dfs map (Pos.offset pos dir) value)
            |> Set.unionMany
        | _ -> Set.empty

    ArrayOfArrays.findIndexes ((=) 0) map
    |> Array.sumBy (fun pos -> dfs map pos -1 |> Set.count)
    // Correct Answer: 682, took: 4ms

let part2 map =
    let rec dfs (map : int [] []) pos prevValue =
        match ArrayOfArrays.tryGet pos map with
        | None -> 0
        | Some 9 when prevValue = 8 -> 1
        | Some value when value = prevValue + 1 ->
            [ -1,0; 0,1; 1,0; 0,-1 ]
            |> List.sumBy (fun dir -> dfs map (Pos.offset pos dir) value)
        | _ -> 0

    ArrayOfArrays.findIndexes ((=) 0) map
    |> Array.sumBy (fun pos -> dfs map pos -1)
    // Correct Answer: 1511, took: 3ms

let map =
    Puzzle.readLinesA "day10.txt"
    |> Array.map Array.ofSeq
    |> ArrayOfArrays.map Char.digitToInt

Puzzle.warmup part1 part2 map // warm it up for more accurate timings
Puzzle.measurePart1ms part1 map
Puzzle.measurePart2ms part2 map