open System
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
    
printf $"Day 1(a): %d{combineVals data}\n"

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
    
printf $"Day 1(b): %d{combineValsTop data}\n"

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
                
printf $"Total: %d{total}\n"

//////////////// Part 2 ///////////////////////
