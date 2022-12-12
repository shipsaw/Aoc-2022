module AdventCode1.Day_5
open System
open System.IO

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
    
printf "Top crate on each stack: %s\n" (String.Concat(Array.ofList(runBulk craneInstructions crateList)))