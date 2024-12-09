#load "lib/Helper.fsx"
open Helper

// Day 8: Resonant Collinearity
//
// Nothing too complex, we simply need to get the difference between two points
// and project them in each direction. We use the combination function to find
// all the combinations of nodes with matching antennas.

let part1 (antennas, grid) =
    let findAntennaAntinodes (nodes : (int * int) []) =
        nodes |> Array.toList
        |> List.combinations 2
        |> List.collect (fun [a; b] ->
            let diff = Pos.difference b a
            [ Pos.offset b diff; Pos.offset a (Pos.neg diff) ])
    
    antennas
    |> Map.mapValues findAntennaAntinodes
    |> Map.collectValues id
    |> Seq.filter (fun pos -> ArrayOfArrays.isPointWithin grid pos)
    |> Seq.distinct |> Seq.length
    // Correct Answer: 308, took: 231µs

let part2 (antennas, grid) =
    let allPointsInDir dir pos =
        List.unfold (fun pos ->
            if ArrayOfArrays.isPointWithin grid pos
            then Some (pos, Pos.offset pos dir)
            else None) pos
        
    let findAntennaAntinodes (nodes : (int * int) []) =
        Array.toList nodes 
        |> List.combinations 2
        |> List.collect (fun [a; b] ->
            let diff = Pos.difference b a
            allPointsInDir diff b @ allPointsInDir (Pos.neg diff) a)
    
    antennas
    |> Map.mapValues findAntennaAntinodes
    |> Map.collectValues id |> Seq.distinct |> Seq.length
    // Correct Answer: 1147, took: 677µs

let grid = Puzzle.readLinesA "day08.txt" |> Array.map Array.ofSeq
let antennas =
    grid
    |> ArrayOfArrays.findIndexesAndValues ((<>) '.')
    |> Array.map Tuple.flip // values by index
    |> Array.groupByTuple |> Map

Puzzle.warmup part1 part2 (antennas, grid) // warm it up for more accurate timings
Puzzle.measurePart1µs part1 (antennas, grid)
Puzzle.measurePart2µs part2 (antennas, grid)
