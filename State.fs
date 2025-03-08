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
    }
    
    let mkState memSize = {
        siMap = Map.empty
        mem = empty memSize
    }
             
    let declare x st =
        if validVariableName x && not (reservedVariableName x)
               && not (Map.containsKey x st.siMap) then
                Some {
                    siMap = st.siMap.Add(x,0)
                    mem = st.mem
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
            }
        else
            None
            
    let alloc x size st =
        match alloc size st.mem with
        | Some (mem',ptr) ->
            let newState = { st with mem = mem' }
            match getVar x newState with
            | Some _ -> setVar x ptr newState
            | None ->
                match declare x newState with
                | Some newNewState -> setVar x ptr newNewState
                | None -> None
        | None -> None
    
    let push _ = failwith "not implemented"
    
    let pop _ = failwith "not implemented"