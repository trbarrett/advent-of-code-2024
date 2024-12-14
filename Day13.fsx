#load "lib/Helper.fsx"
open Helper

// Day 13: Claw Contraption
//
// Use super basic linear algebra to figure out how many button presses for A
// and B are needed to get to the result

let roundn (x : float) (d : int) = System.Math.Round(x, d)

let gaussianElimination (ax: float, ay) (bx, by) (x, y) =
    // solve for a and b in
    // a*x1 + b*x2 = x
    // a*y1 + b*y2 = y
    let ratio = ax/ay
    let by = bx - (by * ratio)
    let y = x - (y * ratio)
    let y = y / by
    let b = y
    let y = y * bx
    let x = x - y
    let a = x / ax
    (a, b)

let part1 clawMachines =
    [ for a, b, result in clawMachines do
        let aPresses, bPresses = gaussianElimination a b result
        if roundn aPresses 3 = round aPresses && roundn bPresses 3 = round bPresses
        then
            let a, b = int64 (round aPresses), int64 (round bPresses)
            yield 3L * a + b]
    |> List.sum
    // Correct Answer: 25751, took: 46µs

let part2 clawMachines =
    [ for a, b, (x, y) in clawMachines do
        let aPresses, bPresses =
            gaussianElimination a b (x + 10000000000000., y + 10000000000000.)
        if roundn aPresses 3 = round aPresses && roundn bPresses 3 = round bPresses
        then
            let a, b = int64 (round aPresses), int64 (round bPresses)
            yield 3L * a + b]
    |> List.sum
    // Correct Answer: 108528956728655, took: 56µs

let clawMachines =
    Puzzle.readLinesL "day13.txt"
    |> splitOnEmptyLines
    |> List.map (fun lines ->
        let [ax; ay] = String.findMatching "\d+" lines.[0]
        let [bx; by] = String.findMatching "\d+" lines.[1]
        let [x; y] = String.findMatching "\d+" lines.[2]
        (float ax, float ay), (float bx, float by), (float x, float y))

Puzzle.warmup part1 part2 clawMachines // warm it up for more accurate timings
Puzzle.measurePart1µs part1 clawMachines
Puzzle.measurePart2µs part2 clawMachines