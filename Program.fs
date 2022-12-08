open System
open System.Collections.Generic
open System.IO
open System.Threading
open Microsoft.FSharp.Core

////////////////// ADVENT DAY 1 ///////////////////////////////

//////////////// Part 1 ///////////////////////
let data = File.ReadAllLines("calories.txt")
            |> Array.map(fun str -> if String.IsNullOrEmpty str then 0 else int str)
            |> Array.toList
    
let combineVals (vals: int list): int =
    let rec g (runningTotal: int) (maxTotal: int) (lst: int list): int =
        match lst with
        | head :: tail ->
            let rt = if head = 0 then 0 else runningTotal + head
            let mt = if head = 0 then max maxTotal runningTotal else maxTotal
            in g rt mt tail
        | [] -> maxTotal
    in g 0 0 vals
    
// printf $"Day 1(a): %d{combineVals data}\n"

//////////////// Part 2 ///////////////////////
let checkNewMax (runningTotal: int) (maxTotal: int[]): int[] =
    if runningTotal <= Array.min maxTotal then maxTotal else
        let sortedArray = Array.sort maxTotal
        in [| runningTotal; sortedArray[1]; sortedArray[2] |]

let combineValsTop (vals: int list): int =
    let rec g (runningTotal: int) (maxTotal: int[]) (lst: int list): int =
        match lst with
        | head :: tail ->
            let rt = if head = 0 then 0 else runningTotal + head
            let mt = if head = 0 then checkNewMax runningTotal maxTotal else maxTotal
            in g rt mt tail
        | [] -> Array.sum maxTotal
    in g 0 [|0; 0; 0|] vals
    
// printf $"Day 1(b): %d{combineValsTop data}\n"

///////////////////// ADVENT DAY 2 ////////////////////////////

//////////////// Part 1 ///////////////////////
type Move =
  | Rock = 1
  | Paper = 2
  | Scissors = 3
  
type Outcome =
    | Won = 6
    | Lost = 0
    | Draw = 3
    
type PlayerMove = PlayerMove of Move
type OpponentMove = OpponentMove of Move

let determineWinner (om: OpponentMove, pm: PlayerMove): Outcome =
    match (pm, om) with
    | PlayerMove Move.Rock,     OpponentMove Move.Rock -> Outcome.Draw
    | PlayerMove Move.Paper,    OpponentMove Move.Paper -> Outcome.Draw
    | PlayerMove Move.Scissors, OpponentMove Move.Scissors -> Outcome.Draw
    | PlayerMove Move.Rock,     OpponentMove Move.Scissors -> Outcome.Won
    | PlayerMove Move.Paper,    OpponentMove Move.Rock -> Outcome.Won
    | PlayerMove Move.Scissors, OpponentMove Move.Paper -> Outcome.Won
    | PlayerMove Move.Rock,     OpponentMove Move.Paper -> Outcome.Lost
    | PlayerMove Move.Paper,    OpponentMove Move.Scissors -> Outcome.Lost
    | PlayerMove Move.Scissors, OpponentMove Move.Rock -> Outcome.Lost
    | _ -> ArgumentOutOfRangeException() |> raise

let gameScore (outcome: Outcome) (playerMove: PlayerMove): int =
    let (PlayerMove move) = playerMove
    int outcome + int move
    
let parseMoves (line: string): OpponentMove * PlayerMove =
    let splitLine = line.Split(' ')
    let om, pm = splitLine[0], splitLine[1]
    let omParsed = match om with
                    | "A" -> OpponentMove Move.Rock
                    | "B" -> OpponentMove Move.Paper
                    | "C" -> OpponentMove Move.Scissors
                    | _ -> ArgumentOutOfRangeException() |> raise
    let pmParsed = match pm with
                    | "X" -> PlayerMove Move.Rock
                    | "Y" -> PlayerMove Move.Paper
                    | "Z" -> PlayerMove Move.Scissors
                    | _ -> ArgumentOutOfRangeException() |> raise
    in (omParsed, pmParsed)
    
let total = File.ReadAllLines("rps.txt")
                |> Array.toList
                |> List.map parseMoves
                |> List.map(fun tup -> gameScore (determineWinner tup) (snd tup) )
                |> List.sum
                
// printf $"Total: %d{total}\n"

//////////////// Part 2 ///////////////////////

let parseMoveAndOutcome (line: string): OpponentMove * Outcome =
    let splitLine = line.Split(' ')
    let om, outcome = splitLine[0], splitLine[1]
    let omParsed = match om with
                    | "A" -> OpponentMove Move.Rock
                    | "B" -> OpponentMove Move.Paper
                    | "C" -> OpponentMove Move.Scissors
                    | _ -> ArgumentOutOfRangeException() |> raise
    let outcomeParsed = match outcome with
                        | "X" -> Outcome.Lost
                        | "Y" -> Outcome.Draw
                        | "Z" -> Outcome.Won
                        | _ -> ArgumentOutOfRangeException() |> raise
    in (omParsed, outcomeParsed)
let determineMove (om: OpponentMove, outcome: Outcome): PlayerMove =
    match (outcome, om) with
    | Outcome.Draw,     OpponentMove Move.Rock -> PlayerMove Move.Rock
    | Outcome.Draw,    OpponentMove Move.Paper -> PlayerMove Move.Paper
    | Outcome.Draw, OpponentMove Move.Scissors -> PlayerMove Move.Scissors
    | Outcome.Won,     OpponentMove Move.Scissors -> PlayerMove Move.Rock
    | Outcome.Won,    OpponentMove Move.Rock -> PlayerMove Move.Paper
    | Outcome.Won, OpponentMove Move.Paper -> PlayerMove Move.Scissors
    | Outcome.Lost,     OpponentMove Move.Paper -> PlayerMove Move.Rock
    | Outcome.Lost,    OpponentMove Move.Scissors -> PlayerMove Move.Paper
    | Outcome.Lost, OpponentMove Move.Rock -> PlayerMove Move.Scissors
    | _ -> ArgumentOutOfRangeException() |> raise

let total2 = File.ReadAllLines("rps.txt")
                |> Array.toList
                |> List.map parseMoveAndOutcome
                |> List.map(fun tup -> gameScore (snd tup) (determineMove tup) )
                |> List.sum
                
// printf $"Total Rigged: %d{total2}\n"

///////////////////// ADVENT DAY 3 ////////////////////////////

//////////////// Part 1 ///////////////////////
let ruksackData = File.ReadAllLines("ruksack.txt")
                |> Array.toList
                
let getIntersect (line: string): char =
    let lineLen = line.Length
    let set1 = Set.ofSeq line[..((lineLen/2) - 1)]
    let set2 = Set.ofSeq line[(lineLen/2)..]
    in (Set.intersect set1 set2).MaximumElement
    
let charToInt (c: char): int =
    let adjustLower = (int 'a') - 1
    let adjustUpper = (int 'A') - 27
    (int c) - (if Char.IsUpper c then adjustUpper else adjustLower)
    
let overlap = ruksackData
            |> List.map getIntersect
            |> List.map charToInt
            |> List.sum

// printf $"Priority sum: %d{overlap}\n"

//////////////// Part 2 ///////////////////////
let getIntersectBadge (lines: string list): int =
    lines
    |> List.map Set.ofSeq
    |> List.reduce(Set.intersect)
    |> Set.maxElement
    |> charToInt

let rec parseGroup (elves: string list) (acc: int): int =
    match elves with
    | [] -> acc
    | elves -> parseGroup (elves[3..]) ((getIntersectBadge elves[0..2]) + acc)
    
let parseBadges (elves: string list): int =
    parseGroup elves 0
    
// printf $"%d{parseBadges ruksackData}"

///////////////////// ADVENT DAY 4 ////////////////////////////

//////////////// Part 1 ///////////////////////

let cleaningData = File.ReadAllLines("cleanup.txt")
                |> Array.toList

let fullOverlap (pairs: int list): bool =
    let elf1: int * int = (pairs[0], pairs[1])
    let elf2: int * int = (pairs[2], pairs[3])
    (fst elf1 >= fst elf2 && snd elf1 <= snd elf2) ||
    (fst elf2 >= fst elf1 && snd elf2 <= snd elf1)
    
let pairsFullOverlap (pairs: string list): int =
    pairs
    |> List.collect (fun x -> List.ofArray (x.Split [|'-'; ','|] ) )
    |> List.map int
    |> List.chunkBySize 4
    |> List.filter fullOverlap
    |> List.length

// printf $"%d{pairsFullOverlap cleaningData}\n"

//////////////// Part 2 ///////////////////////
let partialOverlap (pairs: int list): bool =
    let elf1: int * int = (pairs[0], pairs[1])
    let elf2: int * int = (pairs[2], pairs[3])
    not (snd elf1 < fst elf2 || snd elf2 < fst elf1)
let pairsPartialOverlap (pairs: string list): int =
    pairs
    |> List.collect (fun x -> List.ofArray (x.Split [|'-'; ','|] ) )
    |> List.map int
    |> List.chunkBySize 4
    |> List.filter partialOverlap
    |> List.length

// printf $"%d{pairsPartialOverlap cleaningData}\n"

///////////////////// ADVENT DAY 5 ////////////////////////////

//////////////// Part 1 ///////////////////////
let crateList = [
    [ 'H'; 'L'; 'R'; 'F'; 'B'; 'C'; 'J'; 'M' ]
    [ 'D'; 'C'; 'Z' ]
    [ 'W'; 'G'; 'N'; 'C'; 'F'; 'J'; 'H' ]
    [ 'B'; 'S'; 'T'; 'M'; 'D'; 'J'; 'P' ]
    [ 'J'; 'R'; 'D'; 'C'; 'N' ]
    [ 'Z'; 'G'; 'J'; 'P'; 'Q'; 'D'; 'L'; 'W' ]
    [ 'H'; 'R'; 'F'; 'T'; 'Z'; 'P' ]
    [ 'G'; 'M'; 'V'; 'L' ]
    [ 'J'; 'R'; 'Q'; 'F'; 'P'; 'G'; 'B'; 'C' ]
]

let craneInstructions = File.ReadAllLines("craneData.txt")
                        |> Array.toList

type Instruction = {
    amt: int
    f: int
    t: int
}
type Crates = char list list

let stringToInstruction (ins: string): Instruction =
    let splitArray = ins.Split(' ')
    { amt = int splitArray[1]; f = (int splitArray[3]) - 1; t = (int splitArray[5]) - 1 }
    
let rec moveCrates (amt: int) (f: char list) (t: char list): char list * char list =
    if amt > 0 then moveCrates (amt - 1) (f.Tail) (f.Head :: t)
    else (f, t)
    
let updateCrates (ins: Instruction) (crates: Crates): Crates =
    let fromStack, toStack = moveCrates ins.amt crates[ins.f] crates[ins.t]
    
    crates
    |> List.mapi (fun i x -> if i = ins.f then fromStack else x)
    |> List.mapi (fun i x -> if i = ins.t then toStack else x)
    
    
let rec executeInstructions (ins: Instruction list) (crate: Crates): Crates =
    match ins with
    | [] -> crate
    | x :: xs -> executeInstructions xs (updateCrates x crate)
    
let run (rawIns: string list) (initCrate: Crates): char list =
    let ins = rawIns |> List.map stringToInstruction
    let finalState = executeInstructions ins initCrate
    in finalState
    |> List.map (fun lst -> lst.Head)
    
// printf "Top crate on each stack: %s\n" (String.Concat(Array.ofList(run craneInstructions crateList)))

//////////////// Part 2 ///////////////////////

let updateCratesBulk (ins: Instruction) (crates: Crates): Crates =
    let movedCrates = crates[ins.f][..ins.amt - 1]
    let fromStack = crates[ins.f][ins.amt..]
    let toStack = movedCrates @ crates[ins.t]
    
    crates
    |> List.mapi (fun i x -> if i = ins.f then fromStack else x)
    |> List.mapi (fun i x -> if i = ins.t then toStack else x)
    
let rec executeInstructionsBulk (ins: Instruction list) (crate: Crates): Crates =
    match ins with
    | [] -> crate
    | x :: xs -> executeInstructionsBulk xs (updateCratesBulk x crate)
    
let runBulk (rawIns: string list) (initCrate: Crates): char list =
    let ins = rawIns |> List.map stringToInstruction
    let finalState = executeInstructionsBulk ins initCrate
    in finalState
    |> List.map (fun lst -> lst.Head)
    
// printf "Top crate on each stack: %s\n" (String.Concat(Array.ofList(runBulk craneInstructions crateList)))
