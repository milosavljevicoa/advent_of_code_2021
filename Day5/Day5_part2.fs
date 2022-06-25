module AdventOfCode_2021.Day5_part2

open System

let split (sep: string) (str: string) = str.Split sep |> Array.toList

let replaceLineEndings (replacementText: string) (str: string) = str.ReplaceLineEndings replacementText

let exclude (sep: string) = List.filter (fun x -> x <> sep)

let replace (oldStr: string) (newStr: string) (str: string) = str.Replace(oldStr, newStr)

let rec elementAt (str: string) = str[1]

type Point = { x: int; y: int }
type Line = { point1: Point; point2: Point }

let convertToLine (points: string) =
    // example points "0,9 -> 1,4"
    points
    |> split (" -> ")
    |> fun points ->
        let convertToPoint =
            split (",")
            >> List.map int
            >> (fun p -> { x = p[0]; y = p[1] })

        { point1 = (convertToPoint points[0])
          point2 = (convertToPoint points[1]) }

let getMinAndMax (a: int) (b: int) : (int * int) =
    let max = max a b
    let min = min a b
    (min, max)

let isLineValid (line: Line) =
    let delta_x =
        abs (line.point1.x - line.point2.x)

    let delta_y =
        abs (line.point1.y - line.point2.y)

    delta_x = 0 || delta_y = 0 || delta_x = delta_y

let convertInput (input: string list) =
    input
    |> List.map convertToLine
    |> List.filter isLineValid

type Coordinate =
    | X of int
    | Y of int

let createSequence (min, max) (coordinate: Coordinate) =
    [ min..max ]
    |> List.map (fun cord ->
        match coordinate with
        | X x -> { x = x; y = cord }
        | Y y -> { x = cord; y = y })


let getPointsFromLine (line: Line) = 
    if (line.point1.x <> line.point2.x) && (line.point1.y <> line.point2.y) then
        let (p1, p2) = (line.point1, line.point2)
        let getRange a b =
            let range = [min a b .. max a b]
            if a < b then range 
            else List.rev range
        let xCoors = getRange p1.x p2.x
        let yCoors = getRange p1.y p2.y  

        (xCoors, yCoors)
        ||> List.map2 (fun x y -> { x = x; y = y })

    else
        (if line.point1.x = line.point2.x then
             getMinAndMax line.point1.y line.point2.y, X line.point1.x
         else
             getMinAndMax line.point1.x line.point2.x, Y line.point1.y)
        ||> createSequence


let updateMap (map: Map<Point, int>) (key: Point) =
    map
    |> Map.change key (function
        | Some v -> Some(v + 1)
        | None _ -> Some(1))

let numberOfOverlappingPoints (lines: Line list) =
    lines
    |> List.collect getPointsFromLine
    |> List.fold updateMap (Map.empty<Point, int>)
    |> Map.filter (fun _ v -> v > 1)
    |> Map.count

[<EntryPoint>]
let main args =
    let result =
        System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input.txt"
//        System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}/example.txt"
        |> Array.toList
        |> convertInput
        |> numberOfOverlappingPoints

    printfn $"%d{result}"
    0
