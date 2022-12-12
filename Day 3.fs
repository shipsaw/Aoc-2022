module AdventCode1.Day_3
open System
open System.IO

///////////////////// ADVENT DAY 3 ////////////////////////////

//////////////// Part 1 ///////////////////////
let ruksackData = File.ReadAllLines("ruksack.txt")
                |> Array.toList
                
let getIntersect (line: string): char =
    let lineLen = line.Length
    let set1 = Set.ofSeq line[..((lineLen/2) - 1)]
    let set2 = Set.ofSeq line[(lineLen/2)..]
    in (Set.intersect set1 set2).MaximumElement
    
let charToInt (c: char): int =
    let adjustLower = (int 'a') - 1
    let adjustUpper = (int 'A') - 27
    (int c) - (if Char.IsUpper c then adjustUpper else adjustLower)
    
let overlap = ruksackData
            |> List.map getIntersect
            |> List.map charToInt
            |> List.sum

// printf $"Priority sum: %d{overlap}\n"

//////////////// Part 2 ///////////////////////
let getIntersectBadge (lines: string list): int =
    lines
    |> List.map Set.ofSeq
    |> List.reduce(Set.intersect)
    |> Set.maxElement
    |> charToInt

let rec parseGroup (elves: string list) (acc: int): int =
    match elves with
    | [] -> acc
    | elves -> parseGroup (elves[3..]) ((getIntersectBadge elves[0..2]) + acc)
    
let parseBadges (elves: string list): int =
    parseGroup elves 0
    
printf $"%d{parseBadges ruksackData}"