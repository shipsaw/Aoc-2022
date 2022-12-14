module AdventCode1.Day_11

open System

type WorryLevel = WorryLevel of int

let relief (WorryLevel level): WorryLevel =
    WorryLevel (int (level / 3))

type OperationType = Add | Multiply

type Monkey = {
    id: int
    startingItems: int list
    operation: OperationType * int
    divisibleTestNum: int
    trueThrowTo: int
    falseThrowTo: int
}

let testParse = [
    "Monkey 0:";
    "Starting items: 54, 61, 97, 63, 74";
    "Operation: new = old * 7";
    "Test: divisible by 17";
    "If true: throw to monkey 5";
    "If false: throw to monkey 3";
]

let parseMonkey (rawInp: string) (monkey: Monkey): Monkey =
    let inpArray = rawInp.Split([|" "; ":"; "," |], StringSplitOptions.None) |> List.ofArray
    match inpArray with
    | ["Monkey"; x] -> { monkey with id = (int x) }
    | "Starting" :: "items" :: items -> { monkey with startingItems = items |> List.map(int) }
    | ["Operation"; "new"; "="; "old"; op; amt ] -> { monkey with  }