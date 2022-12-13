module AdventCode1.Day_9

open System
open System.IO

////////////////// ADVENT DAY 9 ///////////////////////////////

//////////////// Part 1 ///////////////////////

type Point = {x: int; y: int}
type Head = Head of Point
type Tail = Tail of Point
let testData = [
    "R 4";
    "U 4";
    "L 3";
    "D 1";
    "R 4";
    "D 1";
    "L 5";
    "R 2";
]

type Direction = Up | Down | Left | Right 
type Instruction = Direction * int
type Bridge = Map<Point, int>

type BridgeState = {
   bridge: Bridge
   head: Head
   tail: Tail
}

let touching (Head head) (Tail tail) =
    abs(head.x - tail.x) <= 1 && abs(head.y - tail.y) <= 1

let moveTail (head: Head) (tail: Tail): Tail =
   match (head, tail) with
   | Head hp, Tail tp when hp.x > tp.x && hp.y > tp.y ->
      Tail { x = tp.x + 1; y = tp.y + 1 }
   | Head hp, Tail tp when hp.x > tp.x && hp.y = tp.y ->
      Tail { tp with x = tp.x + 1 }
   | Head hp, Tail tp when hp.x > tp.x && hp.y < tp.y ->
      Tail { x = tp.x + 1; y = tp.y - 1 }
   | Head hp, Tail tp when hp.x = tp.x && hp.y > tp.y ->
      Tail { tp with y = tp.y + 1 }
   | Head hp, Tail tp when hp.x = tp.x && hp.y < tp.y ->
      Tail { tp with y = tp.y - 1 }
   | Head hp, Tail tp when hp.x < tp.x && hp.y > tp.y ->
      Tail { x = tp.x - 1; y = tp.y + 1 }
   | Head hp, Tail tp when hp.x < tp.x && hp.y = tp.y ->
      Tail { tp with x = tp.x - 1 }
   | Head hp, Tail tp when hp.x < tp.x && hp.y < tp.y ->
      Tail { x = tp.x - 1; y = tp.y - 1 }
   | _ -> raise (InvalidOperationException("Error moving the tail"))
   
let logTail (bridge: Bridge) (Tail tail): Bridge =
   match Map.tryFind tail bridge with
   | Some v -> Map.add tail (v + 1) bridge
   | None -> Map.add tail 0 bridge
   
let parseInstruction (rawIns: string): Instruction =
   let instructionArray = rawIns.Split(' ')
   match instructionArray with
   | [|"U"; x|] -> (Up, (int x))
   | [|"D"; x|] -> (Down, (int x))
   | [|"R"; x|] -> (Right, (int x))
   | [|"L"; x|] -> (Left, (int x))
   | _ -> raise (ArgumentException("Invalid instruction"))
   
let updateState (bridgeState: BridgeState): BridgeState =
   let newTail =
      if not (touching bridgeState.head bridgeState.tail)
      then moveTail bridgeState.head bridgeState.tail
      else bridgeState.tail
   let newBridge = logTail bridgeState.bridge newTail
   { bridge = newBridge; head = bridgeState.head; tail = newTail }
   
let rec moveHead (bridgeState: BridgeState) (instruction: Instruction): BridgeState =
   let (Head point) = bridgeState.head
   match (fst instruction, snd instruction) with
   | _,0 -> bridgeState
   | Left, chg ->
      let newHead = Head { point with x = point.x - 1 }
      let newState = updateState { bridgeState with head = newHead }
      moveHead newState (Instruction (fst instruction, chg - 1))
   | Right, chg ->
      let newHead = Head { point with x = point.x + 1 }
      let newState = updateState { bridgeState with head = newHead }
      moveHead newState (Instruction (fst instruction, chg - 1))
   | Up, chg ->
      let newHead = Head { point with y = point.y + 1 }
      let newState = updateState { bridgeState with head = newHead }
      moveHead newState (Instruction (fst instruction, chg - 1))
   | Down, chg ->
      let newHead = Head { point with y = point.y - 1 }
      let newState = updateState { bridgeState with head = newHead }
      moveHead newState (Instruction (fst instruction, chg - 1))

let rec runInstructions (bs: BridgeState) (instructions: Instruction list): BridgeState =
   match instructions with
   | [] -> bs
   | x :: xs -> runInstructions (moveHead bs x) xs
   
let run (rawIns: string list): int =
   let instructionList =
      rawIns |> List.map parseInstruction
   let initBridge = Map.empty<Point, int>
   let initHead = Head {x = 0; y = 0}
   let initTail = Tail {x = 0; y = 0}
   let initState = { bridge = initBridge; head = initHead; tail = initTail }
   let finalState = (runInstructions initState instructionList).bridge
   finalState |> Map.toList |> List.length

let data = File.ReadAllLines("bridge.txt")
            |> Array.toList
            
let finalOutput =
   printf $"%d{run data}\n"
