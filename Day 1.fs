module AdventCode1.Day_1

open System
open System.IO

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
    
let execute =
    printf $"Day 1(b): %d{combineValsTop data}\n"