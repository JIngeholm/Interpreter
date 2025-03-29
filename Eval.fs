module Interpreter.Eval

    open System
    open Result
    open Language
    open StateMonad
              
              
    let rec readInt() =
     let input = System.Console.ReadLine().Trim()
     match System.Int32.TryParse input with
      | (true,result) -> result
      | (false,_) ->
          printfn "%s is not an integer" input
          readInt()
          
     
     
     
     
    let rec arithEval a  =
        match a with
        | Num x -> ret x
        | Var v -> getVar v
        | Add(b,c) ->
            arithEval c >>= fun cv ->
            arithEval b >>= fun bv ->
            ret (bv + cv)
        | Mul(b,c) ->
            arithEval c >>= fun cv ->
            arithEval b >>= fun bv ->
            ret (bv * cv)
        | Div(b,c) ->
            arithEval c >>= fun cv ->
            if cv = 0 then
                fail
            else
                arithEval b >>= fun bv ->
                ret ( bv / cv)
        | Mod(b,c) ->
            arithEval c >>= fun cv ->
            if cv = 0 then
                fail
            else
                arithEval b >>= fun bv ->
                ret ( bv % cv)
        | MemRead e1 ->
            arithEval e1 >>= fun ptr -> getMem ptr
        | Random ->
            random
        | Read ->
            ret ( readInt() )
        | Cond(b, a1, a2) ->
            boolEval b >>= fun bv ->
            if bv = true then
                arithEval a1
            else
                arithEval a2
        | FunctionCall(s,_) -> failwith "not implemented" 
    and boolEval b  =
        match b with
        | TT -> ret true
        | Eq(a,c) ->
            arithEval a >>= fun av ->
            arithEval c >>= fun cv ->
            ret ( av = cv )
        | Lt(a,c) ->
            arithEval a >>= fun av ->
            arithEval c >>= fun cv ->
            ret ( av < cv )
        | Conj(a,c) ->
            boolEval a >>= fun av ->
            boolEval c >>= fun cv ->
            ret ( av && cv )
        | Not a ->
            boolEval a >>= fun av ->
            ret ( not av )
    
    
    
    
    
    
    let rec arithEval2 a  =
        match a with
        | Num x -> eval { return x }
        | Var v -> getVar v
        | Add(b,c) ->
            eval {
                let! bv = arithEval2 b
                let! cv = arithEval2 c
                return bv + cv
            }
        | Mul(b,c) ->
            eval {
                let! bv = arithEval2 b
                let! cv = arithEval2 c
                return bv * cv
            }
        | Div(b,c) ->
            eval {
                let! cv = arithEval2 c
                if cv = 0 then
                    return! fail
                else
                    let! bv = arithEval2 b
                    return bv + cv
            }
        | Mod(b,c) ->
            eval {
                let! cv = arithEval2 c
                if cv = 0 then
                    return! fail
                else
                    let! bv = arithEval2 b
                    return bv % cv
            }
        | MemRead e1 ->
            eval{
                let! ptr = arithEval2 e1
                return! getMem ptr
            }
        | Random ->
            random
        | Read ->
            eval{ return readInt() }
        | Cond(b, a1, a2) ->
            eval{
                let! bv = boolEval2 b
                if bv = true then
                    return! arithEval2 a1
                else
                    return! arithEval2 a2
            }
        | FunctionCall(s,_) ->
            failwith "not implemented" 
    and boolEval2 b  =
        match b with
        | TT -> eval{ return true }
        | Eq(a,c) ->
            eval{
                let! av = arithEval2 a
                let! cv = arithEval2 c
                return av = cv
            }
        | Lt(a,c) ->
            eval{
                let! av = arithEval2 a
                let! cv = arithEval2 c
                return av < cv
            }
        | Conj(a,c) ->
            eval{
                let! av = boolEval2 a
                let! cv = boolEval2 c
                return av && cv
            }
        | Not a ->
            eval{
                let! av = boolEval2 a
                return not av
            }
    
    
    
    
    
    
    
    (*
    It didn't really make sense to me to use the split function to reconstruct
    the string in the mergeString function. Using substrings seemed much easier and simpler.
    *)
    let rec mergeString es (s: string)   =
        let rec aux es (acc: string) =
            match es with
            | [] -> eval { return acc }
            | x :: xs ->
                eval{
                    let! xv = arithEval2 x
                    let i = acc.IndexOf("%")
                    if i >= 0 then
                        let before = acc.Substring(0,i)
                        let after = acc.Substring(i+1)
                        let acc' = before + (string xv) + after
                        return! aux xs acc'
                    else
                        return acc
                }
        aux es s
        
    
    
    
    
    
    
    
    let rec stmntEval s  =
        match s with
        | Skip -> eval { return () }
        | Declare v -> eval{ return! declare v } 
        | Assign(v,a) ->
            eval{
                let! x = arithEval2 a
                return! setVar v x
            }
        | Seq(s1,s2) ->
            eval{
                do! stmntEval s1
                do! stmntEval s2
            }
        | If(guard,s1,s2) ->
            eval{
                let! bv = boolEval2 guard
                if bv then
                    return! stmntEval s1
                else
                    return! stmntEval s2
            }
        | While(guard,s') ->
            eval{
                let! bv = boolEval2 guard
                if bv then
                    do! stmntEval s'
                    return! stmntEval (While(guard,s'))
                else
                    return ()
            }
        | Alloc(x,e) ->
            eval{
                let! ptr = getVar x
                let! size = arithEval2 e
                return! alloc x size
            }
        | Free(e1,e2) ->
            eval{
                let! ptr = arithEval2 e1
                let! size = arithEval2 e2
                return! free ptr size
            }
        | MemWrite(e1,e2) ->
            eval{
                let! ptr = arithEval2 e1
                let! v = arithEval2 e2
                return! setMem ptr v
            }
        | Print(es, s) ->
            eval{
                let! s' = mergeString es s
                printfn "%s" s'
                return ()
            }
        | Return _ ->
            failwith "not implemented" 
            
    
    
    
    
    
    let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList