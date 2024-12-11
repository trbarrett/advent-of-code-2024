#load "lib/Helper.fsx"
open System.Collections.Generic
open Helper

// Day 11: Plutonian Pebbles
//
// Part 1 - Did it the naivé way of expanding out the full rules then counting
// Part 2 - Did it the faster way of using memoization to remember the amount
//          of expansions for a stone at a given level, and did one stone
//          expansion at a time

let part1 stones =
    let rec blink stone =
        let strLength = int32 (log10 (float stone)) + 1
        match stone with
        | 0L -> [ 1L ]
        | stone when strLength % 2 = 0 ->
            let left = stone / (pown 10L (strLength/2))
            let right = stone % (pown 10L (strLength/2))
            [ right; left ]
        | stone -> [ stone * 2024L ]

    let calcSteps = List.replicate 25 (List.collect blink)
    let calc = List.reduce (>>) calcSteps
    calc stones |> List.length
    // Correct Answer: 175006, took: 16ms

let part2 stones =
    let cache = new Dictionary<_,_>()

    let rec replicate count stone =
        let cacheKey = string count + ":" + string stone
        if count = 0 then 1L
        elif cache.ContainsKey cacheKey then cache.[cacheKey]
        else
            let strLength = int32 (log10 (float stone)) + 1
            match stone with
            | 0L -> replicate (count - 1) 1L
            | stone when strLength % 2 = 0 ->
                let left = stone / (pown 10L (strLength/2))
                let right = stone % (pown 10L (strLength/2))
                let result = replicate (count-1) left + replicate (count-1) right
                cache.[cacheKey] <- result
                result
            | stone ->
                let result = replicate (count-1) (stone * 2024L)
                cache.[cacheKey] <- result
                result

    stones |> List.sumBy (replicate 75)
    // Correct Answer: 207961583799296, took: 25ms

let stones =
    Puzzle.readText "day11.txt" |> String.findMatching "\d+" |> List.map int64

Puzzle.warmup part1 part2 stones // warm it up for more accurate timings
Puzzle.measurePart1ms part1 stones
Puzzle.measurePart2ms part2 stones