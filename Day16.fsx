#load "lib/Helper.fsx"
#load "lib/AStar.fsx"
open Helper
open AStar

// Day 16: Reindeer Maze
//
// Part 1 - aStar, with facings being part of each node and costing
// Part 2 - Nasty hacky solution:
//          Take the astar result path from part1 and one at a time try blocking
//          each position on the path and seeing if running astar on the new
//          maze would have the same score. If it does we have a new path.
//
//          It's possible that there would be variations of variations with an
//          equally good path, but for my data set this worked fine.
//
//          A good optimization would have been to only block the path at a
//          junction, but this is fast enough for me for now. Pure djiksra would
//          probably be faster as well. Oh well.

type Facing = | N | E | S | W

module Facing =
    let dir = function
        | N -> -1, 0 | E -> 0, 1 | S -> 1, 0 | W -> 0, -1
    let turnLeft = function | N -> W | W -> S | S -> E | E -> N
    let turnRight = function | N -> E | E -> S | S -> W | W -> N

type Pos =
    | Pos of (Facing * (int * int))
    | End of (int * int)

let neighbours maze pos =
    match pos with
    | End _ -> []
    | Pos (straightFacing, pos) ->
        let ccwFacing = Facing.turnLeft straightFacing
        let cwFacing = Facing.turnRight straightFacing
        [ straightFacing, Pos.offset pos (Facing.dir straightFacing), 1.0
          ccwFacing, Pos.offset pos (Facing.dir ccwFacing), 1001.0
          cwFacing, Pos.offset pos (Facing.dir cwFacing), 1001.0 ]
        |> List.choose (fun (facing, pos, score) ->
            maze |> ArrayOfArrays.tryGet pos
            |> Option.bind (function
                | 'E' -> Some (End pos, score)
                | '.' -> Some (Pos (facing, pos), score)
                | _ -> None))

let heuristic pos goal =
   match pos with
   | End _ -> 0.
   | Pos (facing, (row, col)) ->
        let (End (goalRowNo, goalColNo)) = goal
        // simple manhattan distance for the heuristic, since no diagonal movement
        (abs (row - goalRowNo) + abs (col - goalColNo)) |> double

let part1 (maze, startPos, endPos) =
    let (Some (_, score)) =
        astar (Pos (E, startPos)) (End endPos) (neighbours maze) heuristic
    score
    // Correct Answer: 95444, took: 77ms

let part2 (maze, startPos, endPos) =
    let (Some (basePath, baseScore)) =
        astar (Pos (E, startPos)) (End endPos) (neighbours maze) heuristic

    basePath
    |> List.choose (fun pos ->
        match pos with
        | End _ -> None
        | Pos (_, pos) ->
            let maze = ArrayOfArrays.set pos '#' maze
            astar (Pos (E, startPos)) (End endPos) (neighbours maze) heuristic
            |> Option.bind (fun (path, newScore) ->
                if newScore = baseScore then Some path else None))
    |> List.collect id
    |> List.append basePath
    |> List.distinctBy (function | End pos -> pos | Pos (_, pos) -> pos)
    |> List.length
    // Correct Answer: 513, took: 10,155ms

let maze = Puzzle.readLinesA "day16.txt" |> Array.map Seq.toArray
let (Some startPos) = ArrayOfArrays.tryFindIndex ((=) 'S') maze
let (Some endPos) = ArrayOfArrays.tryFindIndex ((=) 'E') maze

Puzzle.measurePart1ms part1 (maze, startPos, endPos)
Puzzle.measurePart2ms part2 (maze, startPos, endPos)