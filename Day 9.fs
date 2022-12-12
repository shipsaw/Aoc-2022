module AdventCode1.Day_9

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

type HorizontalSide
    = Left
    | CenterH
    | Right
    
type VerticalSide
    = Up
    | CenterV
    | Down
    
type RelationToTail = HorizontalSide * VerticalSide

type Bridge = bool list list

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
   | _ -> raise (System.InvalidOperationException("Error moving the tail"))
   
let logTail (bridge: Bridge) (Tail tail): Bridge =
   let tailToI = bridge.Length - tail.y 
   let tailToJ = bridge[0].Length - tail.x
   
   bridge
   |> List.mapi(fun i ->
      List.mapi(fun j col ->
         if i = tailToI && j = tailToJ then true else col))