module Interpreter.Eval_yellow_red

open Result
open Language
open State_yellow_red


let rec arithEval a st =
    match a with
    | Num x -> Ok x
    | Var v -> getVar v st
    | Add(b, c) ->
        match (arithEval b st, arithEval c st) with
        | (Ok x, Ok y) -> Ok (x + y)
        | (Error e, _) | (_, Error e) -> Error e
    | Mul(b, c) ->
        match (arithEval b st, arithEval c st) with
        | (Ok x, Ok y) -> Ok (x * y)
        | (Error e, _) | (_, Error e) -> Error e
    | Div(b, c) ->
        match (arithEval b st, arithEval c st) with
        | (Ok x, Ok 0) -> Error DivisionByZero
        | (Ok x, Ok y) -> Ok (x / y)
        | (Error e, _) | (_, Error e) -> Error e
    | Mod(b, c) ->
        match (arithEval b st, arithEval c st) with
        | (Ok x, Ok 0) -> Error DivisionByZero
        | (Ok x, Ok y) -> Ok (x % y)
        | (Error e, _) | (_, Error e) -> Error e


let rec boolEval b st =
    match b with
    | TT -> Ok true
    | Eq(a, c) ->
        match (arithEval a st, arithEval c st) with
        | (Ok valX, Ok valY) -> Ok (valX = valY)
        | (Error e, _) | (_, Error e) -> Error e
    | Lt(a, c) ->
        match (arithEval a st, arithEval c st) with
        | (Ok valX, Ok valY) -> Ok (valX < valY)
        | (Error e, _) | (_, Error e) -> Error e
    | Conj(a, c) ->
        match (boolEval a st, boolEval c st) with
        | (Ok boolX, Ok boolY) -> Ok (boolX && boolY)
        | (Error e, _) | (_, Error e) -> Error e
    | Not a ->
        match boolEval a st with
        | Ok x -> Ok (not x)
        | Error e -> Error e


let rec stmntEval s st =
    match s with
    | Skip -> Ok st
    | Declare v -> declare v st
    | Assign(v, a) ->
        match arithEval a st with
        | Ok x -> setVar v x st
        | Error e -> Error e
    | Seq(s1, s2) ->
        match stmntEval s1 st with
        | Ok st' -> stmntEval s2 st'
        | Error e -> Error e
    | If(guard, s1, s2) ->
        match boolEval guard st with
        | Ok true -> stmntEval s1 (push st) |> Result.bind pop
        | Ok false -> stmntEval s2 (push st) |> Result.bind pop
        | Error e -> Error e
    | While(guard, s') ->
        let rec loop st =
            match boolEval guard st with
            | Ok true ->
                match stmntEval s' (push st) |> Result.bind pop with
                | Ok st' -> loop st'
                | Error e -> Error e
            | Ok false -> Ok st
            | Error e -> Error e
        loop st