module Interpreter.Eval

    open Result
    open Language
    open State                                             
               
    let rec arithEval a st =
        match a with
        | Num x -> Some x
        | Var v -> getVar v st
        | Add(b,c) ->
            arithEval c st
            |> Option.bind
                   (fun cv -> arithEval b st
                           |> Option.map (fun bv -> bv + cv))
        | Mul(b,c) ->
            arithEval c st
            |> Option.bind
                   (fun cv -> arithEval b st
                           |> Option.map (fun bv -> bv * cv))
        | Div(b,c) ->
            arithEval c st
            |> Option.bind
                   (fun cv -> if cv = 0 then None
                              else arithEval b st
                              |> Option.map (fun bv -> bv / cv))
        | Mod(b,c) ->                                                           
            arithEval c st
            |> Option.bind
                   (fun cv -> if cv = 0 then None
                              else arithEval b st
                              |> Option.map (fun bv -> bv % cv))
        | _ -> Some 0
        
    let rec boolEval b st =
        match b with
        | TT -> Some true
        | Eq(a,c) ->
            match arithEval a st, arithEval c st with
            | Some valX, Some valY -> Some(valX = valY)
            | _ -> None
        | Lt(a,c) ->
            match arithEval a st, arithEval c st with
            | Some valX, Some valY -> Some(valX < valY)
            | _ -> None
        | Conj(a,c) ->
            match boolEval a st, boolEval c st with
            | Some boolX, Some boolY -> Some(boolX && boolY)
            | _ -> None
        | Not a ->
            match boolEval a st with
            | Some x -> Some(not x)
            | _ -> None
    
    let rec stmntEval s st =
        match s with
        | Skip -> Some st
        | Declare v -> declare v st
        | Assign(v,a) ->
            match arithEval a st with
            | Some x -> setVar v x st
            | None -> None
        | Seq(s1,s2) ->
            match stmntEval s1 st with
            | None -> None
            | Some st' -> stmntEval s2 st'
        | If(guard, s1, s2) ->
            match boolEval guard st with
            | Some true -> stmntEval s1 st
            | Some false -> stmntEval s2 st
            | None -> None
        | While(guard, s') ->
            match boolEval guard st with
            | Some true ->
                match stmntEval s' st with
                | Some st' -> stmntEval (While(guard,s')) st'
                | None -> None
            | Some false -> Some st
            | None -> None
        | _ -> Some st