module AdventOfCode_2021.Day2_part2
let example = @"
forward 5
down 5
forward 8
up 3
down 8
forward 2
"

type Command =
    | Forward of int
    | Down of int
    | Up of int
    
type Submarine = { HorizontalPosition: int; Depth: int; Aim: int }

let input =
//    example.Trim().Split "\n"
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
     
let getCommand (command: string) =
    let [|rawCommand; rawMovement|] = command.Split " "
    let command =
        match rawCommand with
            | "forward" -> Forward
            | "down" -> Down
            | "up" -> Up
            | _ -> failwith "Invalid command"
    rawMovement |> int |> command
    
let applyCommand state =
    function
        | Forward x -> { state with HorizontalPosition = state.HorizontalPosition + x;
                                     Depth = state.Depth + state.Aim * x}
        | Up x -> { state with Aim = state.Aim - x }
        | Down x -> { state with Aim = state.Aim + x }

let solve (input: string[]) =
    input
    |> Seq.map getCommand
    |> Seq.fold applyCommand { HorizontalPosition = 0; Depth = 0; Aim = 0}
    |> (fun sub -> sub.Depth * sub.HorizontalPosition)
    
solve input
