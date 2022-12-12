module AdventCode1.Day_9

open System

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

type Instruction = Up of int | Down of int | Left of int | Right of int
type Bridge = Map<Point, int>

type BridgeState = {
   bridge: Bridge
   head: Head
   tail: Tail
}

let moveHead (Head head) (instruction: Instruction): Head =
   match instruction with
   | Left chg -> Head { head with x = head.x - 1 }
   | Right chg -> Head { head with x = head.x + 1 }
   | Up chg -> Head { head with y = head.y + 1 }
   | Down chg -> Head { head with y = head.y - 1 }

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
      Tail { tp with y = tp.y - 1 }
   | Head hp, Tail tp when hp.x < tp.x && hp.y < tp.y ->
      Tail { tp with y = tp.y - 1 }
   | _ -> raise (InvalidOperationException("Error moving the tail"))
   
let logTail (bridge: Bridge) (Tail tail): Bridge =
   match Map.tryFind tail bridge with
   | Some v -> Map.add tail (v + 1) bridge
   | None -> Map.add tail 0 bridge
   
let parseInstruction (rawIns: string): Instruction =
   let instructionArray = rawIns.Split(' ')
   match instructionArray with
   | [| _; x |] when not (box x :? int) -> raise (ArgumentException("Invalid instruction"))
   | [|"U"; x|] -> Up (int x)
   | [|"D"; x|] -> Down (int x)
   | [|"R"; x|] -> Right (int x)
   | [|"L"; x|] -> Left (int x)
   | _ -> raise (ArgumentException("Invalid instruction"))
   
let runInstruction (bridgeState: BridgeState) (instruction: Instruction) (head: Head) (tail: Tail): BridgeState =
   let newHead = moveHead head instruction
   let newTail = if not (touching newHead tail) then moveTail newHead tail else tail
   let newBridge = logTail bridgeState.bridge newTail
   { bridge = newBridge; head = newHead; tail = newTail }
   
let rec runInstructions (bs: BridgeState) (instructions: Instruction list): BridgeState =
   match instructions with
   | [] -> bs
   | s :: ss -> runInstructions (runInstruction bs s bs.head bs.tail) ss
   
let run (rawIns: string list): int =
   let instructionList =
      rawIns |> List.map parseInstruction
   let initBridge = Map.empty<Point, int>
   let initHead = Head {x = 0; y = 0}
   let initTail = Tail {x = 0; y = 0}
   let initState = { bridge = initBridge; head = initHead; tail = initTail }
   let finalState = (runInstructions initState instructionList).bridge
   finalState |> Map.toList |> List.filter(fun kvp -> (snd kvp) > 1) |> List.length

printf "Length: %d\n" (run testData)