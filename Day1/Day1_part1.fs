module AdventOfCode_2021.Day1_part1
let example =
    @"199
200
208
210
200
207
240
269
260
263" .Split "\n"

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
    
let solve input =
    input
    |> Seq.map int
    |> Seq.pairwise
    |> Seq.filter (fun (el1, el2) -> el1 < el2)
    |> Seq.length
   
solve input