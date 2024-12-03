#load "./Helper.fsx"
open Helper

// Day 3: Mull It Over
//
// Part 1 - Capture a simple regex match, then multiply the capture groups
// Part 2 - Same as part one, but disabling or enabling counts based on
//          additional captures

let part1 text =
    String.captureAll "mul\((\d+),(\d+)\)" text
    |> List.sumBy (fun [x; y] -> int64 x * int64 y)
    // Correct Answer: 192767529, took: 750µs

let part2 text =
    String.captureAll "mul\((\d+),(\d+)\)|(do\(\))|(don't\(\))" text
    |> List.fold (fun (enabled, acc) m ->
        match enabled, m with
        | _, [_;_;"do()";_] -> (true, acc)
        | _, [_;_;_;"don't()"] -> (false, acc)
        | true, [x;y;_;_] -> (true, acc + (int64 x * int64 y))
        | _ -> (enabled, acc)) (true, 0L)
    |> snd
    // Correct Answer: 104083373, took: 2,294µs

let text = Puzzle.readText "day03.txt"

Puzzle.warmup part1 part2 text // warm it up for more accurate timings
Puzzle.measurePart1µs part1 text
Puzzle.measurePart2µs part2 text
