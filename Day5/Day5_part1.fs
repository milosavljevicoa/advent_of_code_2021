module AdventOfCode_2021.Day5_part1

let split (sep: string) (str: string) = str.Split sep |> Array.toList

let replaceLineEndings (replacementText: string) (str: string) = str.ReplaceLineEndings replacementText

let exclude (sep: string) = List.filter (fun x -> x <> sep)

let replace (oldStr: string) (newStr: string) (str: string) = str.Replace(oldStr, newStr)

let rec elementAt (str: string) = str[1]

type Point = { x: int; y: int }
type Line = { point1: Point; point2: Point }
exception DiagonalLine

let convertToLine (points: string) =
    // example points "0,9 -> 1,4"
    points
    |> split (" -> ")
    |> fun points ->
        let convertToPoint = split (",") >> List.map int >> (fun p -> { x = p[0]; y = p[1] })
        { point1 = (convertToPoint points[0]); point2 = (convertToPoint points[1]) }

let isLineHorizontal (line: Line) = line.point1.x = line.point2.x
let isLineVertical (line: Line) = line.point1.y = line.point2.y 

let convertInput (input: string list) =
    input
    |> List.map convertToLine
    |> List.filter (fun line -> isLineHorizontal line || isLineVertical line)

let getMaxAndMin (a: int) (b: int) : (int * int) =
    let max = max a b
    let min = min a b
    (max, min)

let getMapOfPoints (line: Line) =
    if line.point1.x = line.point2.x then
        let max, min = getMaxAndMin line.point1.y line.point2.y
        let x = line.point1.x 

        [ min..max ] |> List.map (fun y -> { x = x; y = y })

    elif line.point1.y = line.point2.y then
        let max, min = getMaxAndMin line.point1.x line.point2.x
        let y = line.point1.y 

        [ min..max ] |> List.map (fun x -> { x = x; y = y })
        
    else
        raise DiagonalLine
    
    
let updateMap (map: Map<Point, int>) (key: Point) =
    map
    |> Map.change key (function
                        | Some v -> Some(v + 1)
                        | None _ -> Some(1))

let numberOfOverlappingPoints (lines: Line list) = 
    lines
    |> List.collect getMapOfPoints
    |> List.fold updateMap (Map.empty<Point, int>)
    |> Map.filter (fun _ v -> v > 1)
    |> Map.count

//[<EntryPoint>]
let main args =
    let result =
        System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input.txt"
        //        System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}/example.txt"
        |> Array.toList
        |> convertInput
        |> numberOfOverlappingPoints

    printfn $"%d{result}"
    0
