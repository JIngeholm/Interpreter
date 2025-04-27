module Interpreter.Program

open Interpreter
open Interpreter.Parser

let rec parseArgs =
    function
    | []                -> Map.empty
    | name::value::rest -> Map.add name (System.Int32.Parse(value)) (parseArgs rest)
    | _                 -> failwith "Invalid input"


[<EntryPoint>]
let main args =
    let m = args.[2..] |> Array.toList |> parseArgs
 
    System.IO.File.ReadAllText(args.[1]) |>
    runProgramParser |>
    Result.map
        (fun (prog, body) ->
            Eval.stmntEval body |>
            StateMonad.evalState
                (State.mkState
                     (m |> Map.tryFind "--memSize" |> Option.defaultValue 0)
                     (m |> Map.tryFind "--seed")
                     prog)) |>
    printfn "\n\n%A"
    0