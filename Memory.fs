module Interpreter.Memory
    
    type memory = {
        mMap : Map<int,int>;
        next : int
    }

    let empty (memSize: int) = {
        mMap = Map.empty
        next = 0
    }

    let alloc size mem =
        let changeMem m x =
            let newMap = Seq.init x (fun i -> (m.next + i,0))|> Map.ofSeq
            {m with mMap = newMap; next = mem.next + size}
        
        if size <= 0 then
            None
        else
            let mem' = changeMem mem size
            Some(mem' , mem.next)
            
        
    
    let free ptr size mem =
        let changeMem m =
            let newMap =
                m.mMap |> Map.filter (fun key _ -> key < ptr || key >= ptr + size)
            {m with mMap = newMap}
        
        if ptr + size - 1 >= mem.next then
            None
        else
            let mem' = changeMem mem 
            Some mem'
            
        
    let setMem ptr v mem =
        if not(Map.containsKey ptr mem.mMap) then
            None
        else
            let newMap = Map.add ptr v mem.mMap
            let newNext = if ptr = mem.next then mem.next + 1 else mem.next
                
            Some {mem with mMap = newMap; next = newNext}
        
    let getMem ptr mem = Map.tryFind ptr mem.mMap