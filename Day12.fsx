#load "lib/Helper.fsx"
open System.Collections.Generic
open Helper

// Day 12: Garden Groups
//
// Part 1 -
// Part 2 -

let bfs start getSurrounding acc accumulateFn =
    let visited = HashSet<_>()
    let rec bfs' frontier acc =
        match frontier with
        | [] -> acc
        | x::frontier ->
           if visited.Contains(x) then bfs' frontier acc
           else
                visited.Add(x) |> ignore
                let surrounding = getSurrounding x
                bfs' (frontier@surrounding) (accumulateFn x surrounding acc)

    let result = bfs' [start] acc
    Set visited, result

let cardinalDirs = [| -1,0; 0,1; 1,0; 0,-1 |]
type Dirs = | N | S | E | W

let part1 (garden : char [] []) =

    // find all the similar plant types around a position in the garden
    let getSurroundingInPlot pos =
        let plantType = ArrayOfArrays.get pos garden
        (Pos.offsetMany pos cardinalDirs)
        |> ArrayOfArrays.getManyi garden
        |> Array.filter (snd >> ((=) plantType))
        |> Array.map fst |> Array.toList

    // find a contiguous plot, including the fence and area details
    let floodFillPlot previouslyVisited pos =
        let accumulatePlant _ surrounding (perimeter, area) =
            perimeter + 4 - List.length surrounding, 1 + area

        let filled, (perimeter, area) =
            bfs pos getSurroundingInPlot (0, 0) accumulatePlant
        Set.union previouslyVisited filled, perimeter * area

    // go through each position in the garden, floodFilling if we haven't
    // already visited that position
    ((Set.empty, 0), garden) ||> ArrayOfArrays.foldi (fun pos (visited, cost) _ ->
        if Set.contains pos visited then (visited, cost)
        else
            let fillVisited, fillCost = floodFillPlot visited pos
            (Set.union fillVisited visited, fillCost + cost))
    |> snd
    // Correct Answer: 1450816, took: 612ms

// given a set of all the fences in a plot (with their facings) this function
// determines how many total sides (runs of contiguous fences facing the same
// way) the plot has
let countPlotFenceSides (fences : Set<Dirs * (int * int)>) =
    let getFencesInDir facing pos dir =
        List.unfold (fun pos ->
            if Set.contains (facing, pos) fences
            then Some ((facing, pos), Pos.offset pos dir)
            else None) pos

    // find runs of fences
    let getRun (facing, pos) =
        let runDirs =
            match facing with
            | S | N -> [0,1; 0,-1] // runs are orthogonal to the facing
            | E | W -> [1,0; -1,0]

        // get matching in dir
        getFencesInDir facing pos runDirs.[0]
        @ getFencesInDir facing pos runDirs.[1]
        |> Set

    // Find all the runs of fences
    ((Set.empty, 0), fences) ||> Set.fold (fun (visited, acc) fence ->
        if Set.contains fence visited then (visited, acc)
        else (Set.union visited (getRun fence), 1 + acc))
    |> snd

let part2 (garden : char [] []) =
    // This finds all the similar plant types around a position in the garden
    let getSurroundingInPlot pos =
        let plantType = ArrayOfArrays.get pos garden
        (Pos.offsetMany pos cardinalDirs)
        |> ArrayOfArrays.getManyi garden
        |> Array.filter (snd >> ((=) plantType))
        |> Array.map fst |> Array.toList

    // find a contiguous plot, including the fence and area details
    let floodFillPlot previouslyVisited pos =
        let accumulatePlant pos surrounding (fences, area) =
            // Given the surrounding plants of the same type,
            // get the directions of plants of a different type
            let differentPlantDirs =
                (Pos.offsetMany pos cardinalDirs)
                |> Array.filter (fun pos -> not (List.contains pos surrounding))

            // record the fence pos and facing for those different plants
            let newFences =
                differentPlantDirs |> Array.map (fun adjacent ->
                    match Pos.difference adjacent pos with
                    | -1,0 -> (N, pos) |  1,0 -> (S, pos)
                    |  0,1 -> (E, pos) | 0,-1 -> (W, pos))
                |> Set
            Set.union newFences fences, area + 1

        let filled, (fencePositions, area) =
            bfs pos getSurroundingInPlot (Set.empty, 0) accumulatePlant

        let fenceCost = countPlotFenceSides fencePositions * area
        Set.union previouslyVisited filled, fenceCost

    // go through each position in the garden, floodFilling if we haven't
    // already visited that position
    ((Set.empty, 0), garden) ||> ArrayOfArrays.foldi (fun pos (visited, cost) _ ->
        if Set.contains pos visited then (visited, cost)
        else
            let fillVisited, fillCost = floodFillPlot visited pos
            (Set.union fillVisited visited, fillCost + cost))
    |> snd
    // Correct Answer: 865662, took: 685ms

let garden =
    Puzzle.readLinesA "day12.txt" |> Array.map Seq.toArray

Puzzle.warmup part1 part2 garden // warm it up for more accurate timings
Puzzle.measurePart1ms part1 garden
Puzzle.measurePart2ms part2 garden