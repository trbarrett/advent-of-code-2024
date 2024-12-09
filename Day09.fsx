#load "lib/Helper.fsx"
open Helper

// Day 9:  Disk Fragmenter
//
// Part 1 - Walk from both ends of the array (recursively), mutating the array to
//          fill gaps on the left with data from the right
// Part 2 - Same as part 1, but when we find a file on the right use a separate
//          function to look for space from the left with which to fill it
//
// I'm not a great fan of this code, but it does the job for now. I would prefer
// to keep track of chunks of file space rather than flattening it. I had an
// initial stab at it, but the F# singly linked List is not a good choice for
// performance. It was taking over 2 seconds for part 1 doing it that way.
//
// Interestingly replacing the -1s with None's to represent empty data, while
// straight forward, is a 10x performance hit!

let part1 disk =
    let disk = Array.copy disk
    let rec fragment (disk : int []) ptrLeft ptrRight =
        if ptrLeft = ptrRight
        then disk
        else
            if disk.[ptrRight] = -1 then fragment disk ptrLeft (ptrRight - 1)
            elif disk.[ptrLeft] <> -1 then fragment disk (ptrLeft + 1) ptrRight
            else
                disk.[ptrLeft] <- disk.[ptrRight]
                disk.[ptrRight] <- -1
                fragment disk (ptrLeft + 1) (ptrRight - 1)
    let disk = fragment disk 0 (Array.length disk - 1)
    (0L, disk)
    ||> Array.foldi (fun i acc x -> if x = -1 then acc else acc + (int64 i * int64 x))
    // Correct Answer: 6395800119709, took: 3ms

let part2 (disk : int []) =
    let disk = Array.copy disk
    let getSpace ptr =
        let rec nextFile ptr =
            if disk.[ptr] = -1 then nextFile (ptr + 1) else ptr
        (nextFile ptr) - ptr
        
    let getFileSize ptr =
        let fileId = disk.[ptr]
        let rec previousEmptySpace ptr =
            if disk.[ptr] = fileId then previousEmptySpace (ptr - 1) else ptr
        ptr - (previousEmptySpace ptr)
        
    let rec findSpaceForFile (disk : int []) ptrLeft ptrRight fileSize =
        if ptrLeft >= ptrRight then disk
        elif disk.[ptrLeft] <> -1 then findSpaceForFile disk (ptrLeft + 1) ptrRight fileSize
        else
            let emptySize = getSpace ptrLeft
            if fileSize <= emptySize then
                System.Array.Copy(disk, ptrRight - fileSize + 1, disk, ptrLeft, fileSize)
                System.Array.Fill(disk, -1, ptrRight - fileSize + 1, fileSize)
                disk
            else
                findSpaceForFile disk (ptrLeft + emptySize) ptrRight fileSize
            
    let rec fragment (disk : int []) ptrLeft ptrRight =
        if ptrLeft >= ptrRight then disk
        elif disk.[ptrRight] = -1 then fragment disk ptrLeft (ptrRight - 1)
        elif disk.[ptrLeft] <> -1 then fragment disk (ptrLeft + 1) ptrRight
        else
            let fileSize = getFileSize ptrRight
            let disk = findSpaceForFile disk ptrLeft ptrRight fileSize
            fragment disk ptrLeft (ptrRight - fileSize)
                    
    let disk = fragment disk 0 (Array.length disk - 1)
    (0L, disk)
    ||> Array.foldi (fun i acc x -> if x = -1 then acc else acc + (int64 i * int64 x))
    // Correct Answer: 6418529470362, took: 46ms

let diskMap =
    Puzzle.readText "day09.txt"
    |> Seq.map Char.digitToInt
    |> Seq.chunkBySize 2
    |> Seq.indexed
    |> Seq.collect (fun (i, diskData) ->
        // spread out the file and empty space data like in the example
        let usedSpace = List.replicate diskData.[0] i
        let freeSpace =
            if Array.length diskData = 1 then []
            else List.replicate diskData.[1] -1
        usedSpace @ freeSpace)
    |> Seq.toArray

Puzzle.warmup part1 part2 diskMap // warm it up for more accurate timings
Puzzle.measurePart1ms part1 diskMap
Puzzle.measurePart2ms part2 diskMap