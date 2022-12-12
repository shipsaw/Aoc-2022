module AdventCode1.Day_6
open System.IO

///////////////////// ADVENT DAY 6 ////////////////////////////

//////////////// Part 1 ///////////////////////

let dataStream = File.ReadAllText("dataStream.txt")

let rec checkRepeat (len: int) (pos: int) (letters: char list) (msg: char list): int =
    let lettersLen = min letters.Length (len - 1)
    if letters.Length = len && (List.distinct letters).Length = len
    then (pos - 1)
    else checkRepeat len (pos + 1)  ((msg.Head :: letters)[..lettersLen]) msg.Tail
    
let runParse (len: int) (msg: string): int =
    let listMsg = List.ofSeq msg
    checkRepeat len 1 [] listMsg
    
printf $"Packet Position: %d{runParse 4 dataStream}\n"
    
//////////////// Part 2 ///////////////////////

printf $"Header Position: %d{runParse 14 dataStream}\n"