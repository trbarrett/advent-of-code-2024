#load "./Helper.fsx"
open Helper

// Day 1: Historian Hysteria
//
// Part 1 - Difference between each item after sorting each list
// Part 2 - Each item in list 1 * occurrences of that item in list 2

let part1 (lstA, lstB) =
    List.zip (List.sort lstA) (List.sort lstB)
    |> List.sumBy (fun (a, b) -> abs (a - b))
    // Correct Answer: 2164381, took: 144µs

let part2 (lstA, lstB) =
    let counts = List.countBy id lstB |> Map
    lstA |> List.sumBy (fun x -> x * (counts |> Map.findOrDefault x 0))
    // Correct Answer: 20719933, took: 290µs

let lines =
    Puzzle.readLines "day01.txt" |> Seq.toList
    |> List.map (String.findMatching "\d+")
    |> List.map (fun [a; b] -> int a, int b )
    |> List.unzip

Puzzle.warmup part1 part2 lines // warm it up for more accurate timings

Puzzle.measurePart1µs part1 lines
Puzzle.measurePart2µs part2 lines
