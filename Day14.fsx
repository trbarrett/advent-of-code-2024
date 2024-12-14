#load "lib/Helper.fsx"
open Helper

// Day 13: Restroom Redoubt
//
// Part 1 - Use modulo to find the position after multiplying velocity by
//          seconds, then split into a 4-tuple for each quadrant
// Part 2 - Same for the positions, but loop through increasing seconds, looking
//          for runs of adjacent items

let part1 ((roomWidth, roomHeight), robots, seconds) =
    let centreX = roomWidth / 2L
    let centreY = roomHeight / 2L
    robots |> List.map (fun ((x, y), (dx, dy)) ->
        (((x + (dx * seconds)) % roomWidth) + roomWidth) % roomWidth,
        (((y + (dy * seconds)) % roomHeight) + roomHeight) % roomHeight)
    |> List.fold (fun (tl, tr, bl, br) (x,y) ->
        if x < centreX && y < centreY then (((x,y)::tl), tr, bl, br)
        elif x > centreX && y < centreY then (tl, ((x,y)::tr), bl, br)
        elif x < centreX && y > centreY then (tl, tr, ((x,y)::bl), br)
        elif x > centreX && y > centreY then (tl, tr, bl, ((x,y)::br))
        else (tl, tr, bl, br)) ([], [], [], [])
    |> (fun (tl, tr, bl, br) ->
        List.length tl * List.length tr * List.length bl * List.length br)
    // Correct Answer: 210587128, took: 125µs

let part2 ((roomWidth, roomHeight), robots) =
    let rec loop seconds =
        let verticalRunCount =
            robots |> List.map (fun ((x, y), (dx, dy)) ->
                (((x + (dx * seconds)) % roomWidth) + roomWidth) % roomWidth,
                (((y + (dy * seconds)) % roomHeight) + roomHeight) % roomHeight)
            |> Set
            |> Set.fold (fun (prev, acc) (x,y) ->
                if acc = 10 then (prev, acc)
                elif (x, y - 1L) = prev then (x,y), acc + 1 else (x,y), 0
            ) ((0L,0L), 0)
            |> snd
        if verticalRunCount = 10 then seconds else loop (seconds + 1L)
    loop 1L
   // Correct Answer: 7286, took: 2,466ms

let robots  =
    Puzzle.readLinesL "day14.txt"
    |> List.map (fun lines ->
        let [x; y; dx; dy] = String.findMatching "-?\d+" lines
        (int64 x, int64 y), (int64 dx, int64 dy))

//let roomSize = (11L, 7L) // example
let roomSize = (101L, 103L)

Puzzle.measurePart1µs part1 (roomSize, robots, 100L)
Puzzle.measurePart2ms part2 (roomSize, robots)