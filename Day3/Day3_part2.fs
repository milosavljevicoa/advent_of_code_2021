module AdventOfCode_2021.Day3_part2

open System
 
let example = @"
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
" 
let input =
//    example.Trim().Split "\n"
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
    |> Array.toList
    
type BinaryInfo = { NumberOfOnes: int; NumberOfZeros: int }

let rec transpose matrix =
    match matrix with
        | (_::_)::_ -> List.map List.head matrix :: transpose (List.map List.tail matrix)
        | _ -> []
    
let getTransposeMatrix (numbers: string list) =
    numbers
    |> List.map (Seq.map id >> Seq.toList)
    |> transpose
    
let fromBinaryToInteger (str: string) = Convert.ToInt32 (str, 2)

let convertToBinaryInfo (groupBy: (char * char list)) =
    match groupBy |> fst with
        | '1' -> { NumberOfOnes = (groupBy |> snd |> List.length); NumberOfZeros= 0  }
        | '0' -> { NumberOfZeros = (groupBy |> snd |> List.length); NumberOfOnes = 0  }
        | _ -> failwith "not a binary num"
    
let sum (binaryInfo: BinaryInfo) (groupBy: char * char list) =
    match groupBy |> fst with
        | '1' -> { NumberOfOnes = binaryInfo.NumberOfOnes + ((groupBy |> snd) |> List.length)
                   NumberOfZeros = binaryInfo.NumberOfZeros }
        | '0' -> { NumberOfZeros = binaryInfo.NumberOfZeros + (groupBy |> snd |> List.length)
                   NumberOfOnes = binaryInfo.NumberOfOnes  }
        | _ -> failwith "not a binary num" 

let getBinaryInfo (input: string list) index =
    input
    |> getTransposeMatrix
    |> List.item index
    |> List.groupBy id
    |> List.fold sum { NumberOfOnes = 0; NumberOfZeros = 0 }

let filter (getDelimiter: BinaryInfo -> char) index (input: string list) =
    let binaryInfo = getBinaryInfo input index
    let numbersToCheck = getDelimiter binaryInfo
    input |> List.filter (fun binaries -> binaries.[index] = numbersToCheck)
  
let rec filterDown (getDelimiter: BinaryInfo -> char) index (input: string list) =
    match input with
        | _::tail when tail |> List.length > 0 ->
            input
            |> filter getDelimiter index
            |> filterDown getDelimiter (index + 1)
        | head::_ -> head
        | _ -> ""
        
let getRating (getDelimiter: BinaryInfo -> char) index=
    filterDown getDelimiter index
    >> fromBinaryToInteger 
  
let oxygenGeneratorRatingDelimiter (binaryInfo: BinaryInfo) =
    if binaryInfo.NumberOfOnes >= binaryInfo.NumberOfZeros then '1' else '0'
    
let co2ScrubberRatingDelimiter (binaryInfo: BinaryInfo) =
    if binaryInfo.NumberOfOnes >= binaryInfo.NumberOfZeros then '0' else '1'
   
let solve input =
    (input |> getRating oxygenGeneratorRatingDelimiter 0) * (input |> getRating co2ScrubberRatingDelimiter 0)

solve input 