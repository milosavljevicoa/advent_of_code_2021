module AdventOfCode_2021.Day3_part1

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
    
let rec transpose matrix =
    match matrix with
        | (_::_)::_ -> List.map List.head matrix :: transpose (List.map List.tail matrix)
        | _ -> []
    
let getTransposeMatrix (numbers: string[]) =
    numbers
    |> Seq.toList
    |> List.map (Seq.map id >> Seq.toList)
    |> transpose
    
let getBinaryDigitInverse =
    function
        | "1" -> "0"
        | "0" -> "1"
        | _ -> failwith "Not binary digit"
    
let concat (str: string list) = String.Concat str 
let fromBinaryToInteger (str: string) = Convert.ToInt32 (str, 2)
    
let solve =
     getTransposeMatrix
    >> List.map (List.groupBy id)
    >> List.map (List.maxBy (fun (_, occ) -> List.length occ))
    >> List.map (fst >> string)
    >> (fun list -> [list; List.map getBinaryDigitInverse list])
    >> List.map (concat >> fromBinaryToInteger)
    >> List.reduce (*)

solve input
