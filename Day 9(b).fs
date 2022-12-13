module AdventCode1.Day_9b

open System
open System.IO

////////////////// ADVENT DAY 9 ///////////////////////////////

//////////////// Part 1 ///////////////////////

type Point = {x: int; y: int}
type Head = Head of Point
type Tail = Tail of Point list
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

let touching (p1: Point) (p2: Point) =
    abs(p1.x - p2.x) <= 1 && abs(p1.y - p2.y) <= 1

let movePoint (p1: Point) (p2: Point): Point =
   match (p1, p2) with
   | p1, p2 when p1.x > p2.x && p1.y > p2.y ->
      { x = p2.x + 1; y = p2.y + 1 }
   | p1, p2 when p1.x > p2.x && p1.y = p2.y ->
      { p2 with x = p2.x + 1 }
   | p1, p2 when p1.x > p2.x && p1.y < p2.y ->
      { x = p2.x + 1; y = p2.y - 1 }
   | p1, p2 when p1.x = p2.x && p1.y > p2.y ->
      { p2 with y = p2.y + 1 }
   | p1, p2 when p1.x = p2.x && p1.y < p2.y ->
      { p2 with y = p2.y - 1 }
   | p1, p2 when p1.x < p2.x && p1.y > p2.y ->
      { x = p2.x - 1; y = p2.y + 1 }
   | p1, p2 when p1.x < p2.x && p1.y = p2.y ->
      { p2 with x = p2.x - 1 }
   | p1, p2 when p1.x < p2.x && p1.y < p2.y ->
      { x = p2.x - 1; y = p2.y - 1 }
   | _ -> raise (InvalidOperationException("Error moving the tail"))
   
let rec moveTail (lead: Point) (rest: Point list) =
   match rest with
   | [] -> [ lead ]
   | x :: xs -> 
         if not (touching lead x)
         then
            let newLead = movePoint lead x
            lead :: (moveTail newLead xs)
         else
            lead :: rest
   
let logTail (bridge: Bridge) (Tail tail): Bridge =
   let tailEnd = List.last tail
   match Map.tryFind tailEnd bridge with
   | Some v -> Map.add tailEnd (v + 1) bridge
   | None -> Map.add tailEnd 0 bridge
   
let parseInstruction (rawIns: string): Instruction =
   let instructionArray = rawIns.Split(' ')
   match instructionArray with
   | [|"U"; x|] -> (Up, (int x))
   | [|"D"; x|] -> (Down, (int x))
   | [|"R"; x|] -> (Right, (int x))
   | [|"L"; x|] -> (Left, (int x))
   | _ -> raise (ArgumentException("Invalid instruction"))
   
let updateState (bridgeState: BridgeState): BridgeState =
   let (Head headPoint) = bridgeState.head
   let (Tail tail) = bridgeState.tail
   
   
   let newTail =
      if not (touching headPoint tail.Head)
      then 
         let tailLead = movePoint headPoint tail.Head
         moveTail tailLead tail.Tail
      else tail
   let newBridge = logTail bridgeState.bridge (Tail newTail)
   { bridge = newBridge; head = bridgeState.head; tail = (Tail newTail) }
   
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
   let initTail = Tail [ for _ in 1 .. 9 -> {x = 0; y = 0 } ]
   let initState = { bridge = initBridge; head = initHead; tail = initTail }
   let finalState = (runInstructions initState instructionList).bridge
   finalState |> Map.toList |> List.length

let data = File.ReadAllLines("bridge.txt")
            |> Array.toList
            
let finalOutput =
   printf $"%d{run data}\n"

