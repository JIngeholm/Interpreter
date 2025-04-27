module Interpreter.StateMonad
    
    open System.Net.Http.Headers
    open State
    open Language
    
    type 'a stateMonad = SM of (state -> ('a * state) option)
        
    let ret x= SM (fun st -> Some(x, st))
    let fail    = SM (fun _ -> None)
    
    let bind (SM f) g =
        SM (fun st ->
            match f st with
            | Some (x, st') -> let (SM h) = g x in h st'
            | None -> None)
        
    let (>>=) a f = bind a f
    let (>>>=) a b = a >>= (fun _ -> b)
    
    let declare str =
        SM ( fun st ->
             match declare str st with
             | Some state' -> Some ((), state')
             | None -> None
           )
        
    let setVar str v =
        SM ( fun st ->
             match setVar str v st with
             | Some state' -> Some((), state')
             | None -> None
           )
    let getVar str =
        SM ( fun st ->
             match getVar str st with
             | Some x -> Some (x,st)
             | None -> None
           )
    
    let alloc str size =
        SM ( fun st ->
             match alloc str size st with
             | Some state' -> Some ((), state')
             | None -> None
           )
   
    let free ptr size =
        SM ( fun st ->
             match free ptr size st with
             | Some state' -> Some ((), state')
             | None -> None
           )
    
    let setMem ptr v =
        SM ( fun st ->
             match setMem ptr v st with
             | Some state' -> Some ((), state')
             | None -> None
           )

    let getMem ptr =
        SM ( fun st ->
             match getMem ptr st with
             | Some x -> Some (x, st)
             | None -> None
           )

    let random  = SM ( fun st -> Some ( random st , st) )
    
    let fork ss =
        SM (fun st ->
            let rec evalAll st monads =
                match monads with
                | [] -> Some((), st)
                | SM m :: rest ->
                    match m st with
                    | Some ((), _) -> evalAll st rest
                    | None -> None
            evalAll st ss
        )        
    
    let evalState st a =
        let (SM f) = a
        match f st with
        | Some(v, _) -> Some v
        | None -> None
        
    type StateBuilder() =  
        member this.Bind(f, x) = (>>=) f x  
        member this.Return(x) = ret x  
        member this.ReturnFrom(x: 'a stateMonad) = x  
        member this.Combine(a, b) = a >>= (fun _ -> b) 
      
    let eval = StateBuilder()