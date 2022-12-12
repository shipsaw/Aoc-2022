module AdventCode1.Day_4
open System
open System.IO

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

printf $"%d{pairsPartialOverlap cleaningData}\n"