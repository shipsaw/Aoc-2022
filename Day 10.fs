module AdventCode1.Day_10

open System
open System.IO

let testData = [
    "addx 15";
    "addx -11";
    "addx 6";
    "addx -3";
    "addx 5";
    "addx -1";
    "addx -8";
    "addx 13";
    "addx 4";
    "noop";
    "addx -1";
    "addx 5";
    "addx -1";
    "addx 5";
    "addx -1";
    "addx 5";
    "addx -1";
    "addx 5";
    "addx -1";
    "addx -35";
    "addx 1";
    "addx 24";
    "addx -19";
    "addx 1";
    "addx 16";
    "addx -11";
    "noop";
    "noop";
    "addx 21";
    "addx -15";
    "noop";
    "noop";
    "addx -3";
    "addx 9";
    "addx 1";
    "addx -3";
    "addx 8";
    "addx 1";
    "addx 5";
    "noop";
    "noop";
    "noop";
    "noop";
    "noop";
    "addx -36";
    "noop";
    "addx 1";
    "addx 7";
    "noop";
    "noop";
    "noop";
    "addx 2";
    "addx 6";
    "noop";
    "noop";
    "noop";
    "noop";
    "noop";
    "addx 1";
    "noop";
    "noop";
    "addx 7";
    "addx 1";
    "noop";
    "addx -13";
    "addx 13";
    "addx 7";
    "noop";
    "addx 1";
    "addx -33";
    "noop";
    "noop";
    "noop";
    "addx 2";
    "noop";
    "noop";
    "noop";
    "addx 8";
    "noop";
    "addx -1";
    "addx 2";
    "addx 1";
    "noop";
    "addx 17";
    "addx -9";
    "addx 1";
    "addx 1";
    "addx -3";
    "addx 11";
    "noop";
    "noop";
    "addx 1";
    "noop";
    "addx 1";
    "noop";
    "noop";
    "addx -13";
    "addx -19";
    "addx 1";
    "addx 3";
    "addx 26";
    "addx -30";
    "addx 12";
    "addx -1";
    "addx 3";
    "addx 1";
    "noop";
    "noop";
    "noop";
    "addx -9";
    "addx 18";
    "addx 1";
    "addx 2";
    "noop";
    "noop";
    "addx 9";
    "noop";
    "noop";
    "noop";
    "addx -1";
    "addx 2";
    "addx -37";
    "addx 1";
    "addx 3";
    "noop";
    "addx 15";
    "addx -21";
    "addx 22";
    "addx -6";
    "addx 1";
    "noop";
    "addx 2";
    "addx 1";
    "noop";
    "addx -10";
    "noop";
    "noop";
    "addx 20";
    "addx 1";
    "addx 2";
    "addx 2";
    "addx -6";
    "addx -11";
    "noop";
    "noop";
    "noop";
]
type Instruction
    = Noop
    | Addx of int

type Cycle = {
    during: int
    after: int
}

let runAddx (newX: int) (oldX: int): Cycle list =
    [{ during = oldX; after = oldX }; { during = oldX; after = oldX + newX }]
    
let runNoop (oldX: int): Cycle list =
    [{ during = oldX; after = oldX }]
    
let decodeInstruction (history: Cycle list) (ins: Instruction): Cycle list =
    let currentX = if history.Length <> 0 then (history |> List.last).after else 1
    match ins with
    | Noop -> List.append history (runNoop currentX) 
    | Addx x -> List.append history (runAddx x currentX) 
let parseInstruction (rawIns: string): Instruction =
    match rawIns.Split(' ') with
    | [| "noop" |] -> Noop
    | [| "addx"; amt |] -> Addx (int amt)
    | _ -> raise (ArgumentException $"Invalid instruction: %s{rawIns}")
    
let rec runInstruction (history: Cycle list) (instructions: Instruction list): Cycle list =
    match instructions with
    | [] -> history
    | x :: xs -> runInstruction (decodeInstruction history x) xs
    
let runStrength (rawIns: string list) =
    let parsedInstructions = rawIns |> List.map parseInstruction
    runInstruction [] parsedInstructions
    |> List.mapi(fun i x -> (i+1) * x.during)
    
let data = File.ReadAllLines("cpuInstructions.txt")
            |> Array.toList
let strengthResults =
    data
    |> runStrength
    
let getSelection =
    printf "%d\n" (strengthResults[19] + strengthResults[59] + strengthResults[99] + strengthResults[139] + strengthResults[179] + strengthResults[219])
    
// Part 2

let runX (rawIns: string list) =
    let parsedInstructions = rawIns |> List.map parseInstruction
    runInstruction [] parsedInstructions
    
let screen = [ 0 .. 239 ] |> List.map(fun x -> x % 40)

let screenRendered (screen: int list) (instructions: int list) =
    List.zip screen instructions
    |> List.map (fun x -> if abs((snd x) -  (fst x)) <= 1 then '#' else '.' )
    |> List.iteri (fun i x -> if (i + 1) % 40 <> 0 then printf $"{x}" else printf $"{x}\n" )

let renderScreen = screenRendered screen (runX data |> List.map(fun x -> x.during))

    
