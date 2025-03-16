module Interpreter.Eval

    open System
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
        | MemRead e1 ->
            arithEval e1 st |> Option.bind (fun ptr -> getMem ptr st)
        | Random ->
            Some(random st)
        | Read ->
            Some ( readInt() )
        | Cond(b, a1, a2) ->
            Some(420)
        
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
        | If(guard,s1,s2) ->
            match boolEval guard st with
            | Some true -> stmntEval s1 st
            | Some false -> stmntEval s2 st
            | None -> None
        | While(guard,s') ->
            match boolEval guard st with
            | Some true ->
                match stmntEval s' st with
                | Some st' -> stmntEval (While(guard,s')) st'
                | None -> None
            | Some false -> Some st
            | None -> None
        | Alloc(x,e) ->
            match getVar x st, arithEval e st with
            | Some ptr, Some size ->
                match alloc x size st with
                    | Some st'' -> Some st''
                    | _ -> None
            | _ -> None
        | Free(e1,e2) ->
            match arithEval e1 st, arithEval e2 st with
            | Some ptr, Some size ->
                match free ptr size st with
                | Some st' -> Some st'
                | _ -> None
            | _ -> None
        | MemWrite(e1,e2) ->
            match arithEval e1 st, arithEval e2 st with
            | Some ptr, Some v ->
                match setMem ptr v st with
                | Some st' -> Some st'
                | _ -> None
            | _ -> None
        | Print(es, s) ->
            match mergeStrings es s st with
            | Some s' ->
                printfn "%s" s'
                Some st
            | None -> None
    
    
    
    let rec readInt() =
     let input = System.Console.ReadLine().Trim()
     match System.Int32.TryParse input with
      | (true,result) -> result
      | (false,_) ->
          printfn "%s is not an integer" input
          readInt()
          
    (*
    It didn't really make sense to me to use the split function to reconstruct
    the string in the mergeString function. Using subsrings seemed much easier and simpler.
    *)
    let rec mergeString es (s: string) st  =
        let rec aux es (acc: string) =
            match es with
            | [] -> Some acc
            | x :: xs ->
                match arithEval x st with
                | Some x' ->
                    let i = acc.IndexOf("%")
                    if i >= 0 then
                        let before = acc.Substring(0,i)
                        let after = acc.Substring(i+1)
                        let acc' = before + (string x') + after
                        aux es acc'
                    else
                        Some acc
                | None -> None
        aux es s
    
    (*
    I don't understand how to use aritEval without a state here, so i just used mkState().
    Also im not sure if the string is still kind of an accumulator.
    *)
    let rec mergeString2 es s =
        let rec aux es (s:string) c =
            match es with
            | [] -> Some (c s)
            | x :: xs ->
                match arithEval x (mkState 0 (Some 0)) with
                | Some x' ->
                    let i = s.IndexOf("%")
                    if i >= 0 then
                        let before = s.Substring(0,i)
                        let after = s.Substring(i+1)
                        let acc' = before + (string x') + after
                        aux es acc' c
                    else
                        Some(c s)
                | None -> Some(c s)
        aux es s id
                    
        
    
    let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList