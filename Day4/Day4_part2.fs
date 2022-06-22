module AdventOfCode_2021.Day4_part2

let trim (str: string) = str.Trim()
let split (delimiter: string) (str: string) = str.Split(delimiter) |> Array.toList

let filterTruthy (str: string list) =
    str |> List.filter (fun str -> str.Length > 0)

let trimAndSplitAndFilterTruthy (delimiter: string) =
    split delimiter >> List.map trim >> filterTruthy

let example =
    @"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
    
22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19
 
 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"
    |> trimAndSplitAndFilterTruthy "\n"

type BingoNumber = { value: int; mutable marked: bool }

type BingoInfo =
    { bingoNumbers: int list
      bingoTables: BingoNumber list list list }

let convertTableToBingoNumber table : BingoNumber list list =
    table
    |> List.map (fun row ->
        row
        |> split (" ")
        |> filterTruthy
        |> List.map (fun value -> { marked = false; value = value |> int }))

let convertTablesToBingoNumber (table: string list) : BingoNumber list list list =
    table

    example
    |> List.tail
    |> List.chunkBySize 5
    |> List.map convertTableToBingoNumber

let convertToBingoReadable table =
    { bingoNumbers =
        table
        |> List.head
        |> trimAndSplitAndFilterTruthy ","
        |> List.map int
      bingoTables = table |> List.tail |> convertTablesToBingoNumber }

let bingoHandler (number: int) (table: BingoNumber list list list) =
    table
    |> List.collect id
    |> List.collect id
    |> List.iter (fun bingoNumber ->
        if bingoNumber.value = number then
            bingoNumber.marked <- true)

let rec transpose matrix =
    match matrix with
    | (_ :: _) :: _ ->
        List.map List.head matrix
        :: transpose (List.map List.tail matrix)
    | _ -> []

let tableStatus (table: BingoNumber list list) =
    table
    |> List.map (List.map (fun bingoNumber -> bingoNumber.marked))
    |> fun table ->
        let bingo =
            List.map (fun row -> row |> List.forall id)
            >> List.contains true

        table |> transpose |> bingo || table |> bingo

let getStatus (tables: BingoNumber list list list) = tables |> List.map tableStatus


let elementAt index (list: 'a list list) = list[index]

let calculateScore (tables: BingoNumber list list) =
    tables
    |> List.collect id
    |> List.filter (fun b -> not b.marked)
    |> List.sumBy (fun b -> b.value)


let printTable (table: BingoNumber list list) =
    for row in table do
        for col in row do
            let marked =
                if col.marked then "True" else "False"

            printf $"%2d{col.value} - %5s{marked}  "

        printfn ""

let playBingo table =
    let bingo = convertToBingoReadable table

    let mutable bingoNumbers =
        bingo.bingoNumbers

    let mutable lastWinner = -1

    let mutable bingoNumber = -1

    let mutable everyOneHasWon = false

    while not everyOneHasWon && bingoNumbers.Length > 0 do
        bingoNumber <- List.head bingoNumbers
        bingoHandler bingoNumber bingo.bingoTables 

        if lastWinner = -1 then
            lastWinner <-
                bingo.bingoTables
                |> getStatus
                |> List.indexed
                |> List.filter (fun (_, hasWon) -> not hasWon)
                |> (fun list ->
                    if list.Length = 1 then
                        list.Head |> fst
                    else
                        -1)
        else
            everyOneHasWon <- bingo.bingoTables |> elementAt lastWinner |> tableStatus 

        bingoNumbers <- List.tail bingoNumbers

    let a =
        bingo.bingoTables
        |> elementAt lastWinner
        |> calculateScore

    bingoNumber * a




[<EntryPoint>]
let main args =
    let input =
        System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input.txt"
        |> Array.toList
        |> List.map trim
        |> filterTruthy

    input |> playBingo |> printfn "Output %d"

    0
