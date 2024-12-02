#load "./Helper.fsx"
open Helper

// Day 2: Red-Nosed Reports
//
// Part 1 - For each report check each pairwise change is in the same safe
//          direction
// Part 2 - Same as part 1, but for each report try removing each level
//          one-by-one and see if any of the results are safe

type PairCategory = | Unsafe | SafeIncreasing | SafeDecreasing

let categorizePair (x, y) =
    match y - x with
    | 1 | 2 | 3 -> SafeIncreasing
    | -1 | -2 | -3 -> SafeDecreasing
    | _ -> Unsafe
    
let isSafe levels =
    List.pairwise levels
    |> List.map categorizePair
    |> List.reduce (fun x s -> if x = s then x else Unsafe)

let part1 reports =
    reports
    |> List.map isSafe
    |> List.filter ((<>) Unsafe)
    |> List.length
    // Correct Answer: 486, took: 481µs

let part2 reports =
    reports
    |> List.filter (fun levels ->
        levels
        |> List.mapi (fun i _ ->  List.removeAt i levels |> isSafe)
        |> List.exists (fun x -> x <> Unsafe))
    |> List.length
    // Correct Answer: 540, took: 3,006µs

let lines =
    Puzzle.readLines "day02.txt" |> Seq.toList
    |> List.map (String.findMatching "\d+" >> List.map int)

Puzzle.warmup part1 part2 lines // warm it up for more accurate timings
Puzzle.measurePart1µs part1 lines
Puzzle.measurePart2µs part2 lines
