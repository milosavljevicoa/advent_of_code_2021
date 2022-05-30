module AdventOfCode_2021.Day4_part1

let trim (str: string) = str.Trim()
let split (delimiter: string) (str: string) = str.Split(delimiter) |> Array.toList
let filterTruthy (strs: string list) = strs |> List.filter (fun str -> str.Length > 0)
let trimAndSplitTruthy = split "\n" >> List.map trim >> filterTruthy

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
    |> trimAndSplitTruthy 
    
type Number = { row: int; col: int; value: int; marked: bool } 

let mapRow (row: int * (int * string) list) =
    row
    |> snd 
    |> List.map (fun (column, number) -> { value = int number; row = fst row; col = column; marked = false })
    
let test = 0, [(0, "22 13 17 11  0"); (1, "8  2 23  4 24"); (2, "21  9 14 16  7"); (3, "6 10  3 18  5"); (4, "1 12 20 15 19")]

mapRow test
            
let parseTable (value: string list list)= 
    value
    |> List.map List.indexed
//    |> List.map List.indexed
//    |> List.indexed
//    |> List.map mapRow
    
let parseBoard (value: string list) =
    value
    |> List.chunkBySize 5
    |> parseTable
    
parseBoard (example |> List.skip 1)