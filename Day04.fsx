#load "lib/Helper.fsx"
open Helper

// Day 4: Ceres Search
//
// Part 1 - Walk in the 8 points around each letter to try to match "XMAS",
//          counting the occurrences
// Part 2 - Match a cross-hatch at each letter, trying to find "MAS", counting
//          the occurrences

let part1 (puzzle : char [] []) =
    let dirs = [ -1,-1; -1,0; -1,1; 0,1; 1,1; 1,0; 1,-1; 0,-1 ]

    puzzle
    |> ArrayOfArrays.mapi (fun pos _ ->
        [| for dir in dirs -> ArrayOfArrays.walk pos dir 4 puzzle |]
        |> Array.filter ((=) [|'X';'M';'A';'S'|])
        |> Array.length)
    |> ArrayOfArrays.sum
    // Correct Answer: 2662, took: 111ms

let part2 puzzle =
    let crossNeg = [|-1,-1; 0,0; 1, 1|]
    let crossPos = [|-1, 1; 0,0; 1,-1|]

    puzzle
    |> ArrayOfArrays.mapi (fun pos _ ->
        [| crossNeg |> Pos.offsetMany pos |> ArrayOfArrays.getMany puzzle
           crossPos |> Pos.offsetMany pos |> ArrayOfArrays.getMany puzzle |]
        |> Array.filter (fun word -> word = [|'M';'A';'S'|] || word = [|'S';'A';'M'|]))
    |> ArrayOfArrays.filter (fun matches -> Array.length matches = 2)
    |> ArrayOfArrays.length
    // Correct Answer: 2034, took: 24ms

let puzzle = Puzzle.readLinesA "day04.txt" |> Array.map Array.ofSeq

Puzzle.warmup part1 part2 puzzle // warm it up for more accurate timings
Puzzle.measurePart1ms part1 puzzle
Puzzle.measurePart2ms part2 puzzle
