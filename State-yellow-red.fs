module Interpreter.State_yellow_red

open Result
open Language

let reservedVariableName v =
    let lst = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]
    List.contains v lst

let validVariableName (v:string) =
    let c = v.[0]
    if (System.Char.IsAsciiLetter c || c = '_')
       && (String.forall (fun x -> x = '_' || System.Char.IsAsciiLetterOrDigit x)) v then
        true
    else
        false

type state = { siStack: Map<string, int> list }

let mkState () = { siStack = [Map.empty] }

let declare x st =
    match st.siStack with
    | top :: rest ->
        if validVariableName x && not (reservedVariableName x) then
            if Map.containsKey x top then
                Error (VarAlreadyExists x)
            else
                Ok { siStack = (Map.add x 0 top) :: rest }
        else
            if reservedVariableName x then
                Error (ReservedName x)
            else if not (validVariableName x) then
                Error (InvalidVarName x)
            else
                Error (VarAlreadyExists x)
    | [] -> failwith "State stack is empty"

let rec getVar x st =
    let rec findVar lst =
        match lst with
        | [] -> Error (VarNotDeclared x)
        | top :: rest ->
            match Map.tryFind x top with
            | Some v -> Ok v
            | None -> findVar rest
    findVar st.siStack

let rec setVar x v st =
    let rec update lst =
        match lst with
        | [] -> Error (VarNotDeclared x)
        | top :: rest ->
            if Map.containsKey x top then
                Ok { siStack = (Map.add x v top) :: rest }
            else
                match update rest with
                | Ok updatedRest -> Ok { siStack = top :: updatedRest.siStack }
                | Error e -> Error e
    update st.siStack


let push st = { siStack = Map.empty :: st.siStack }

let pop st = { siStack = List.tail st.siStack }