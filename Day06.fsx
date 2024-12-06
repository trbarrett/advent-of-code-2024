#load "lib/Helper.fsx"
open System.Collections.Generic
open Helper

// Day 6: Guard Gallivant
//
// Part 1 - Simple recursive walk through the map until we hit the bounds,
//          remembering each step
// Part 2 - Taking the result from part 1, for each position on the path
//          block it then walk the path again to see if we have a loop.

type Guard = { Pos : int * int; Facing : int * int }

module Guard =
    let stepForward guard = { guard with Pos = Pos.offset guard.Pos guard.Facing}
    let turnRight guard =
        match guard.Facing with
        | -1, 0 -> { guard with Facing = 0, 1 }
        | 0, 1 -> { guard with Facing = 1, 0 }
        | 1, 0 -> { guard with Facing = 0, -1 }
        | 0, -1 -> { guard with Facing = -1, 0 }
        | _ -> failwith $"Unknown facing {guard.Facing}"

let rec search lab guard history =
    let next = lab |> ArrayOfArrays.tryGet (Guard.stepForward guard).Pos
    match next with
    | None -> guard::history // we've walked off the map
    | Some '#' -> search lab (Guard.turnRight guard) history
    | _ -> search lab (Guard.stepForward guard) (guard::history)

let part1 (lab, guardStart) =
    search lab guardStart [] |> List.distinctBy _.Pos |> List.length
    // Correct Answer: 4758, took: 983µs

let rec checkHasLoop lab guard (history : HashSet<_>) =
    history.Add(guard) |> ignore
    let guardNext = Guard.stepForward guard
    let next = ArrayOfArrays.tryGet guardNext.Pos lab
    match next with
    | None -> false // we've walked off the map without looping
    | Some '#' ->
        checkHasLoop lab (Guard.turnRight guard) history
    | _ ->
        if history.Contains guardNext then true // found a repeat
        else checkHasLoop lab (Guard.stepForward guard) history

let part2 (lab, guardStart) =
    search lab guardStart [] |> List.map _.Pos |> List.distinct
    |> List.filter (fun possiblePos ->
        let lab = ArrayOfArrays.set possiblePos '#' lab
        checkHasLoop lab guardStart (HashSet()))
    |> List.length
    // Correct Answer: 1670, took: 2,178ms

let lab = Puzzle.readLinesA "day06.txt" |> Array.map Array.ofSeq
let guardStart =
    { Pos = lab |> ArrayOfArrays.findIndexes ((=) '^') |> Array.head
      Facing = (-1, 0) }

Puzzle.warmup part1 part2 (lab, guardStart) // warm it up for more accurate timings
Puzzle.measurePart1µs part1 (lab, guardStart)
Puzzle.measurePart2ms part2 (lab, guardStart)
