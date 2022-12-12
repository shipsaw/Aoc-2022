module AdventCode1.Day_7
open System.IO

///////////////////// ADVENT DAY 7 ////////////////////////////

//////////////// Part 1 ///////////////////////

let commands = File.ReadAllLines("commands.txt")
            |> Array.toList
            
type ObjType
    = File
    | Directory

type FsObject = {
    name: string
    size: bigint
    objType: ObjType
    parent: FsObject option
}

type Status = {
    objMap: Map<string, FsObject>
    input: string list
    currNode: FsObject
}

type Operation
    = ChangeNode of string
    | GetChildren
    | DefineObject of FsObject

exception InvalidLine of string
let parseLine (line: string): Operation =
    let tokens = line.Split(' ')
    let mutable res: bigint = 0
    match tokens with
    | [|"$"; "ls"|] -> GetChildren
    | [|"$"; "cd"; name |] -> ChangeNode name
    | [|"dir"; name|] -> DefineObject { name = name; size = 0; objType = Directory; parent = None }
    | [|num; name|] when bigint.TryParse(num, &res) -> DefineObject { name = name; size = res; objType = File; parent = None }
    | _ -> raise (InvalidLine $"Invalid line provided: {line}")

let rec getDirectoryPath (obj: FsObject): string =
    match obj.parent with
    | None -> obj.name
    | Some p -> getDirectoryPath p + obj.name
    
let addDirectory (s: Status) (name: string): Status =
    let fullPath = s.currNode.name + name + "/"
    let newDirectory = { objType = Directory; name = fullPath; size = 0; parent = Some s.currNode }
    let updatedMap = if s.objMap.ContainsKey fullPath then s.objMap
                     else s.objMap.Add (fullPath, newDirectory)
    in { s with objMap = updatedMap; currNode = newDirectory }                     
    
let rec propagateFile (s: Status) (size: bigint) (nodePtr: FsObject): Status =
     let oldObject = s.objMap[nodePtr.name]
     let newMap = s.objMap
                     |> Map.remove nodePtr.name
                     |> Map.add nodePtr.name { oldObject with size = oldObject.size + size }
     match nodePtr.parent with
     | None -> { s with objMap = newMap }
     | Some p -> propagateFile { s with objMap = newMap } size p
    
    
let processFile (s: Status) (obj: FsObject): Status =
    match obj.objType with
    | File -> propagateFile s obj.size s.currNode
    | Directory -> s
    
let doOperation (s: Status) : Status =
    let operation = parseLine s.input.Head
    match operation with
    | ChangeNode ".." -> { s with currNode = s.currNode.parent.Value }
    | ChangeNode x -> addDirectory s x
    | DefineObject fsObject -> processFile s { fsObject with parent = Some s.currNode }
    | _ -> s
    
let rec processLines (s: Status): Status =
    match s.input with
    | [] -> s
    | _ :: xs -> let newStatus = doOperation s
                 in processLines { newStatus with input = xs }
                  
let processCommands (lines: string list): Status =
    let baseNode: FsObject = { name = "/"; size = 0; objType = Directory; parent = None }
    let initStatus = { objMap = Map.ofList [ "/", baseNode ]; input = lines.Tail; currNode = baseNode }
    processLines initStatus
    
let totalToDelete =     
    Map.toList (processCommands commands).objMap
    |> List.filter (fun kvp -> (snd kvp).objType = Directory)
    |> List.filter(fun kvp -> (snd kvp).size <= 100000)
    |> List.sumBy(fun kvp -> (snd kvp).size)
    
printf "Total: %O\n" totalToDelete

//////////////// Part 2 ///////////////////////

let totalSpace: bigint = 70000000
let unusedSpace: bigint = 30000000
let directorySizes =
                Map.toList (processCommands commands).objMap
                |> List.filter (fun kvp -> (snd kvp).objType = Directory)
                |> List.map (fun kvp -> (snd kvp).size)
                
let rootSize = List.max directorySizes                
let spaceToFree = unusedSpace - (totalSpace - rootSize)
let smallestNeededDirectory =
                directorySizes
                |> List.filter (fun s -> s >= spaceToFree)
                |> List.min
                
printf $"Smallest directory needed: {smallestNeededDirectory}\n"