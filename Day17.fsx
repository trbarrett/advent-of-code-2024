#load "lib/Helper.fsx"
open Helper

// Day 17: Chronospatial Computer
//
// Part 1 - Write an opcode evaluator and run it
// Part 2 -
//   * First I coded up a way to print what the opcode evaluator was doing
//   * Looking through it I wrote out the operations and noticed it was
//     operating on base-8.
//   * Then I noticed that it was only looking at the last base-8 column of RegA
//     each loop, and each loop we got rid of that last column and repeated it
//     until we ran out of base-8 digits
//   * I saw that the calculations for RegB and RegC were purely based of RegA
//     and mostly looking at the last few digits
//   * I simplified the way the program ran to get rid of B and C registers so
//     it was a single calculation on A for each loop
//   * I figured out that I could work backwards starting at an empty RegA and
//     figure out what value would give us the last output. Then I could shift
//     that value left and start again.
//   * I looked for a purely mathematical solution for a while, but realized
//     that for each position/column I would have to try all values 0-7 to see
//     what gave me a valid value we needed for the output
//   * For each stage we take all the possible A values and do a DFS for the
//     remaining values. Since we are trying 0..7 we will find the smallest
//     valid result first

type ProgramState =
    { RegA : int64
      RegB : int64
      RegC : int64
      InstrPtr : int }

type OpCode = | ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV

module OpCode =
    let fromInt = function
        | 0 -> ADV | 1 -> BXL | 2 -> BST | 3 -> JNZ | 4 -> BXC | 5 -> OUT | 6 -> BDV | 7 -> CDV

let combo programState n =
    match n with
    | 0 | 1 | 2 | 3 -> int64 n
    | 4 -> programState.RegA
    | 5 -> programState.RegB
    | 6 -> programState.RegC
    | _ -> failwithf $"Code: {n} is not valid for a comboOperand"

let processInstr (program : int []) state =
    let operand = program.[state.InstrPtr + 1]
    match OpCode.fromInt (program.[state.InstrPtr]) with
    | ADV -> { state with RegA = state.RegA / (pown 2L (int (combo state operand)))
                          InstrPtr = state.InstrPtr + 2 }, []
    | BXL -> { state with RegB = state.RegB ^^^ operand
                          InstrPtr = state.InstrPtr + 2 }, []
    | BST -> { state with RegB = (combo state operand) % 8L
                          InstrPtr = state.InstrPtr + 2 }, []
    | JNZ -> if state.RegA = 0
             then { state with InstrPtr = state.InstrPtr + 2 }, []
             else { state with InstrPtr = operand }, []
    | BXC -> { state with RegB = state.RegB ^^^ state.RegC
                          InstrPtr = state.InstrPtr + 2 }, []
    | OUT -> { state with InstrPtr = state.InstrPtr + 2 },
                [string ((combo state operand) % 8L)]
    | BDV -> { state with RegB = state.RegA / (pown 2L (int (combo state operand)))
                          InstrPtr = state.InstrPtr + 2 }, []
    | CDV -> { state with RegC = state.RegA / (pown 2L (int (combo state operand)))
                          InstrPtr = state.InstrPtr + 2 }, []

let part1 (startingState, program : int []) =
    let rec loop state output =
        if state.InstrPtr >= program.Length then state, output
        else
            let newState, out = processInstr program state
            loop newState (out@output)
    let state, output = loop startingState []
    String.concat "," output
    // Correct Answer: 4,0,4,7,1,2,7,1,6 took: 920µs

let printCombo programState n =
    match n with
    | 0 | 1 | 2 | 3 -> $"CMB (-): '{n}'"
    | 4 -> $"CMB (A): %o{programState.RegA}"
    | 5 -> $"CMB (B): %o{programState.RegB}"
    | 6 -> $"CMB (C): %o{programState.RegC}"
    | _ -> failwithf $"Code: {n} is not valid for a comboOperand"

let printInstr (program : int []) state =
    let operand = program.[state.InstrPtr + 1]
    match OpCode.fromInt (program.[state.InstrPtr]) with
    | ADV ->
        let result = state.RegA / (pown 2L (int (combo state operand)))
        printfn $"%2d{state.InstrPtr} ADV = RegA %o{state.RegA}"
        printfn $"          / 2^ {printCombo state operand}"
        printfn $"      -> RegA %o{result}"
    | BXL ->
        let result = state.RegB ^^^ operand
        printfn $"%2d{state.InstrPtr} BXL = RegB %o{state.RegB}"
        printfn $"          xor '{operand}'"
        printfn $"      -> RegB %o{result}"
    | BST ->
        let result = (combo state operand) % 8L
        printfn $"%2d{state.InstrPtr} BST = {printCombo state operand} %% 8"
        printfn $"      -> RegB %o{result}"
    | JNZ ->
        if state.RegA = 0
        then printfn $"%2d{state.InstrPtr} JNZ = RegA %o{state.RegA}"
             printfn $"      -> NO JMP"
        else printfn $"%2d{state.InstrPtr} JNZ = RegA %o{state.RegA}"
             printfn $"      -> JMP '{operand}'"
    | BXC ->
        let result = state.RegB ^^^ state.RegC
        printfn $"%2d{state.InstrPtr} BXC = RegB %o{state.RegB}"
        printfn $"          xor RegC %o{state.RegC}"
        printfn $"      -> RegB %o{result}"
    | OUT ->
        let result = (combo state operand) % 8L
        printfn $"%2d{state.InstrPtr} OUT = {printCombo state operand} %% 8 "
        printfn $"      -> OUT {result}"
    | BDV ->
        let result = state.RegA / (pown 2L (int (combo state operand)))
        printfn $"%2d{state.InstrPtr} BDV = RegA {state.RegA}"
        printfn $"          / 2^ {printCombo state operand}"
        printfn $"      -> RegB %o{result}"
    | CDV ->
        let result = state.RegA / (pown 2L (int (combo state operand)))
        printfn $"%2d{state.InstrPtr} BDV = RegA {state.RegA}"
        printfn $"          / 2^ {printCombo state operand}"
        printfn $"      -> RegC {result}"

// Debugging helper
let printExecution (startingState, program : int []) =
    let rec loop state output =
        if state.InstrPtr >= program.Length then state, output
        else
            printInstr program state
            let newState, out = processInstr program state
            loop newState (out@output)
    let _, output = loop startingState []
    String.concat "," output

let solveForNextOutput state wantedOutput =
    // Hard coded logic for the test program
    [ for aRemainder in [0L..7L] do
        let part1 = (aRemainder ^^^ 1) ^^^ 4
        let nextA = (state.RegA <<< 3) + aRemainder
        let part2 = nextA / (pown 2L ((int aRemainder) ^^^ 1))
        if (part1 ^^^ part2) % 8L = int64 wantedOutput
        then yield nextA ]

let part2 (startingState, program : int []) =
    let rec loop state programRev =
        match programRev with
        | [] -> Some state.RegA
        | next::programRev ->
            solveForNextOutput state next
            |> List.tryPick (fun a -> loop { state with RegA = a } programRev)

    loop { startingState with RegA = 0L } (List.ofArray program |> List.rev)
    // Correct Answer: 202322348616234, took: 635µs

let startingState, program =
    let [regA; regB; regC; _; program] = Puzzle.readLinesL "day17.txt"

    let program = program |> String.findMatching "\d+" |> List.map int |> Array.ofList
    { RegA = regA |> String.findMatching "\d+" |> List.head |> int64
      RegB = regB |> String.findMatching "\d+" |> List.head |> int64
      RegC = regC |> String.findMatching "\d+" |> List.head |> int64
      InstrPtr = 0 },
    program

Puzzle.measurePart1µs part1 (startingState, program)
Puzzle.measurePart2µs part2 (startingState, program)