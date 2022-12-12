module AdventCode1.Day_8
open System
open System.IO

///////////////////// ADVENT DAY 8 ////////////////////////////

//////////////// Part 1 ///////////////////////

type TreeData = (int option) list list
let treeData: TreeData =
             File.ReadAllLines("trees.txt")
             |> Array.toList
             |> List.map(List.ofSeq)
             |> List.map(List.map(fun c -> Some ((int c) - (int '0'))))

let sightLine (lst: int option list): int option list =
    let rec g (lst: int option list) (state: int option) =
        if lst = [] then [] else
            match (lst.Head, state) with
            | Some x, Some y when x > y -> lst.Head :: (g lst.Tail lst.Head)
            | _ -> None :: (g lst.Tail state)
    in g lst (Some -1)
    
let combineCell (x: int option) (y: int option): int option =
    match (x, y) with
    | Some x, _ -> Some x
    | _, Some y -> Some y
    | _, _ -> None
    
let combineViews (tree1row: int option list) (tree2row: int option list): int option list =
    List.map2 combineCell tree2row tree1row
        
let leftView (treeData: TreeData): TreeData =
    treeData
    |> List.map(sightLine)
    
let rightView (treeData: TreeData): TreeData =
    treeData
    |> List.map(List.rev)
    |> List.map(sightLine)
    |> List.map(List.rev)
    
let topView (treeData: TreeData): TreeData =
    treeData
    |> List.transpose
    |> List.map(sightLine)
    |> List.transpose
    
let bottView (treeData: TreeData): TreeData =
    treeData
    |> List.transpose
    |> List.map(List.rev)
    |> List.map(sightLine)
    |> List.map(List.rev)
    |> List.transpose
    
let combinedView (treeData: TreeData): TreeData =
    let left = leftView treeData
    let right = rightView treeData
    let top = topView treeData
    let bottom = bottView treeData
    let leftRight = (left, right) ||> List.map2(combineViews)
    let topBottom = (top, bottom) ||> List.map2(combineViews)
    let final = (leftRight, topBottom) ||> List.map2(combineViews)
    final
    
let visibleTrees = treeData
                   |> combinedView
                   |> List.concat
                   |> List.filter(Option.isSome)
                   |> List.length
                
printf "Visible trees: %d\n" visibleTrees

//////////////// Part 2 ///////////////////////

// 4; 5; 3; 7; 5; 4; 

type sightState
    = Alive
    | Blocked
let sightFromState (lst: int list) (treeHeight: int): int =
    let rec g (lst: int list) (th: int) (state: sightState) (cnt: int) =
        // printf $"state is {state}, list is {lst}, count is {cnt}\n"
                match (lst, th, state, cnt) with
                | [], _, _, cnt -> cnt
                | l, th, Alive, cnt when th > lst.Head -> g l.Tail th Alive (cnt + 1)
                | l, th, Alive, cnt when th <= lst.Head -> g l.Tail th Blocked (cnt + 1)
                | _ -> cnt
    in g lst treeHeight Alive 0
    
let stringToNumberList (trees: string list): int list list =
    trees
    |> List.map(List.ofSeq)
    |> List.map(List.map(fun c -> (int c) - (int '0')))
    
let getVisibilityLtoR (trees: int list list): int list list =
    trees
    |> List.map (fun row -> List.mapi( fun i -> sightFromState row[i+1..]) row)
    
let visibilityLR (trees: int list list): int list list =
    trees
    |> getVisibilityLtoR
    
let visibilityRL (trees: int list list): int list list =
    trees
    |> List.map(List.rev)
    |> getVisibilityLtoR
    |> List.map(List.rev)
    
let visibilityTB (trees: int list list): int list list =
    trees
    |> List.transpose
    |> getVisibilityLtoR
    |> List.transpose
    
let visibilityBT (trees: int list list): int list list =
    trees
    |> List.transpose
    |> List.map(List.rev)
    |> getVisibilityLtoR
    |> List.map(List.rev)
    |> List.transpose
    
let rec totalVisibility (trees: string list): int list list =
    let tempTrees = trees |> stringToNumberList
    
    let leftRight = tempTrees |> visibilityLR
    let rightLeft = tempTrees |> visibilityRL
    let topBottom = tempTrees |> visibilityTB
    let bottomTop = tempTrees |> visibilityBT
    
    let totalLR = List.mapi(fun i -> List.mapi(fun j col -> col * rightLeft[i][j])) leftRight
    let totalTB = List.mapi(fun i -> List.mapi(fun j col -> col * bottomTop[i][j])) topBottom
    List.mapi(fun i -> List.mapi(fun j col -> col * totalLR[i][j])) totalTB
    
let treeData2 = File.ReadAllLines("trees.txt") |> Array.toList
             
treeData2
|> totalVisibility
|> List.map(List.max)
|> List.max
|> printf "Max visibility: %d\n"