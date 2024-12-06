#load "lib/Helper.fsx"
open Helper

// Day 5: Print Queue
//
// Create a map of all the pages that must precede another. Then go through each
// page in an update to see if the rule is followed by each succeeding page.
// For part two we move the incorrectly succeeding pages before the page we're
// checking if the rule is broken, then check again recursively.

let rec fixPages precedingPagesRules remainingPages =
    match remainingPages with
    | [] -> []
    | x::xs ->
        let mustPrecede = precedingPagesRules |> Map.findOrDefault Set.empty x
        let preceding, valid =
            xs |> List.partition (fun x -> Set.contains x mustPrecede)
        if not (List.isEmpty preceding)
        then fixPages precedingPagesRules (preceding @ [x] @ valid)
        else x :: (fixPages precedingPagesRules xs)

let part1 (precedingPagesRules, updates) =
    updates
    |> List.filter (fun pages -> fixPages precedingPagesRules pages = pages)
    |> List.sumBy (fun pages -> pages.[pages.Length/2])
    // Correct Answer: 5108, took: 4,501µs

let part2 (precedingPagesRules, updates) =
    updates
    |> List.choose (fun pages ->
        let fix = fixPages precedingPagesRules pages
        if fix = pages then None else Some fix)
    |> List.sumBy (fun pages -> pages.[pages.Length/2])
    // Correct Answer: 7380, took: 4,326µs

let problem =
    Puzzle.readLines "day05.txt"
    |> splitOnEmptyLines
    |> fun [pageRules; pages] ->
        pageRules
        |> List.map (String.findMatching "\d+")
        |> List.map (fun [a;b] -> int b, int a) // flip to get precedents
        |> List.groupByTuple |> Map
        |> Map.mapValues Set,
        pages |> List.map (String.findMatching "\d+" >> List.map int)

Puzzle.warmup part1 part2 problem // warm it up for more accurate timings
Puzzle.measurePart1µs part1 problem
Puzzle.measurePart2µs part2 problem
