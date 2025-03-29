module Interpreter.State

    open Interpreter.Memory
    open Result
    open Language
    
    let reservedVariableName v =
        let lst = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]
        List.contains v lst
        
    let validVariableName (v:string) =
        let c = v.[0]
        (System.Char.IsAsciiLetter c || c = '_') && (String.forall (fun x -> x = '_' || System.Char.IsAsciiLetterOrDigit x)) v
    
    type state = {
        siMap: Map<string,int>
        mem : memory
        rng: System.Random
    }
    
    let mkState memSize (oseed: int option) (prog: program) = {
        siMap = Map.empty
        mem = empty memSize
        rng = match oseed with
              | Some seed -> System.Random(seed)
              | None -> System.Random()
    }
    
    let random st = st.rng.Next()
             
    let declare x st =
        if validVariableName x && not (reservedVariableName x)
               && not (Map.containsKey x st.siMap) then
                Some {
                    siMap = st.siMap.Add(x,0)
                    mem = st.mem
                    rng = st.rng
                }
            else
                None
    
    let getVar x st = Map.tryFind x st.siMap
    
    let setVar x v st =
        let f x =
            match x with
            | Some _ -> Some v
            | None -> None
        if Map.containsKey x st.siMap then
            Some {
                siMap = st.siMap.Change (x,f)
                mem = st.mem
                rng = st.rng
            }
        else
            None
            
    let alloc x size st =
        match alloc size st.mem with
        | Some (mem',ptr) ->
            let newState = { st with mem = mem'; siMap = st.siMap }
            match getVar x newState with
            | Some _ -> setVar x ptr newState
            | None ->
                match declare x newState with
                | Some newNewState -> setVar x ptr newNewState
                | None -> None
        | None -> None
    
    let free ptr size st =
        match free ptr size st.mem with
        | Some mem' -> Some { st with mem = mem' }
        | None -> None
        
    let setMem ptr v st =
        match setMem ptr v st.mem with
        | Some mem' -> Some { st with mem = mem' }
        | None -> None
        
    let getMem ptr st = getMem ptr st.mem
    
    
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"
    let pushFrame _ = failwith "not implementd"
    let popFrame _ = failwith "not implemented"