module AdventCode1.Day_2
open System
open System.IO

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
                
printf $"Total Rigged: %d{total2}\n"