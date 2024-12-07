#load "lib/Helper.fsx"
open System.Collections.Generic
open Helper

// Day 7: Bridge Repair
//
// Part 1 - Simple fold performing each operation (+,*), the number of possible
//          solutions expands at each fold iteration by 2^n
// Part 2 - Same as part 1, but adding a concatenation, taking it to a 3^n
//          expansion for each fold

let part1 calibrations =
    calibrations
    |> List.filter (fun (testValue, numbers) ->
        ([List.head numbers], List.tail numbers)
        ||> List.fold (fun totals n ->
            totals |> List.collect (fun t -> [t + n; t * n]))
        |> List.contains testValue)
    |> List.sumBy fst
    // Correct Answer: 1298103531759 took: 7ms

let part2 calibrations =
    calibrations
    |> List.filter (fun (testValue, numbers) ->
        ([List.head numbers], List.tail numbers)
        ||> List.fold (fun totals n ->
            totals |> List.collect (fun t ->
                let significantDigits = int32 (log10 (float n)) + 1
                [ t + n; t * n; (t * (pown 10L significantDigits)) + n])
            |> List.filter (fun x -> x <= testValue))
        |> List.contains testValue)
    |> List.sumBy fst
    // Correct Answer: 140575048428831 took: 217ms

let calibrations =
    Puzzle.readLinesL "day07.txt"
    |> List.map (String.findMatching "\d+" >> List.map int64)
    |> List.map (fun (x::xs) -> x,xs)

Puzzle.warmup part1 part2 calibrations // warm it up for more accurate timings
Puzzle.measurePart1ms part1 calibrations
Puzzle.measurePart2ms part2 calibrations
