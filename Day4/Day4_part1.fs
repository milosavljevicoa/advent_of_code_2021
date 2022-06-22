module AdventOfCode_2021.Day4_part1

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

type BingoNumber =
    { row: int
      col: int
      value: int
      mutable marked: bool }

type BingoInfo =
    { bingoNumbers: int list
      bingoTables: BingoNumber list }

let convertRowToBingoNumber (row, column) =
    column
    |> split (" ")
    |> filterTruthy
    |> List.indexed
    |> List.map (fun (column, value) ->
        { row = row
          col = column
          marked = false
          value = value |> int })

let convertTableToBingoNumber (table: string list) : BingoNumber list =
    table
    |> List.indexed
    |> List.collect convertRowToBingoNumber

let convertToBingoReadable table =
    { bingoNumbers =
        table
        |> List.head
        |> trimAndSplitAndFilterTruthy ","
        |> List.map int
      bingoTables = table |> List.tail |> convertTableToBingoNumber }

let bingoHandler (number: int) (table: BingoNumber list) =
    table
    |> List.iter (fun bingoNumber ->
        if bingoNumber.value = number then
            bingoNumber.marked <- true)


let rec transpose matrix =
    match matrix with
    | (_ :: _) :: _ ->
        List.map List.head matrix
        :: transpose (List.map List.tail matrix)
    | _ -> []

let tableStatus (table: BingoNumber list) =
    table
    |> List.map (fun bingoNumber -> bingoNumber.marked)
    |> List.chunkBySize 5
    |> fun table ->
        let bingo =
            List.map (fun row -> row |> List.forall id)
            >> List.contains true

        table |> transpose |> bingo || table |> bingo

let hasWinner (tables: BingoNumber list) =
    tables
    |> List.chunkBySize 25
    |> List.map tableStatus


let elementAt index (list: 'a list list) = list[index]

let calculateScore winnerIndex (tables: BingoNumber list) =
    tables
    |> List.chunkBySize 25
    |> elementAt winnerIndex
    |> List.filter (fun b -> not b.marked)
    |> List.sumBy (fun b -> b.value)


let playBingo table =
    let bingo = convertToBingoReadable table

    let mutable bingoNumbers =
        bingo.bingoNumbers

    let mutable winner = -1

    let mutable bingoNumber = -1

    while winner < 0 && bingoNumbers.Length > 0 do
        bingoNumber <- List.head bingoNumbers
        bingoHandler bingoNumber bingo.bingoTables

        winner <-
            bingo.bingoTables
            |> hasWinner
            |> List.tryFindIndex id
            |> (fun opt -> if opt.IsSome then opt.Value else -1)

        bingoNumbers <- List.tail bingoNumbers

    let a = calculateScore winner bingo.bingoTables
    bingoNumber
    * a

//[<EntryPoint>]
let main args =
     let input =
        System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input.txt"
        |> Array.toList
        |> List.map trim |> filterTruthy

     input |> playBingo |> printfn "Output %d"
     0