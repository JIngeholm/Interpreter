module Interpreter.Memory

open Interpreter.Language 

module memoryModule =

    type memory = {
        mMap : Map<int,int>
        next : int
    }
    let empty (memSize: int) = {
        mMap = Map.empty
        next = 0
    }

    let alloc size mem =
        let changeMem m x =
            let newMap = 
                Seq.init x (fun i -> (m.next + i, 0)) 
                |> Map.ofSeq
                |> Map.fold (fun acc k v -> Map.add k v acc) m.mMap
            { m with mMap = newMap; next = m.next + size }
        if size <= 0 then None
        else
            let mem' = changeMem mem size
            Some (mem', mem.next)

    let free ptr size mem =
        let changeMem m =
            let newMap = 
                m.mMap 
                |> Map.filter (fun key _ -> key < ptr || key >= ptr + size)
            { m with mMap = newMap }
        if ptr + size - 1 >= mem.next then None
        else
            let mem' = changeMem mem
            Some mem'

    let setMem ptr v mem =
        if not (Map.containsKey ptr mem.mMap) then
            None
        else
            let newMap = Map.add ptr v mem.mMap
            let newNext = if ptr = mem.next then mem.next + 1 else mem.next
            Some { mem with mMap = newMap; next = newNext }

    let getMem ptr mem = Map.tryFind ptr mem.mMap


// ------------------------------------------------------------
// New parallel-safe memory using a MailboxProcessor (actor model)

type message =
    | Alloc of int * AsyncReplyChannel<(memoryModule.memory * int) option>
    | Free of int * int * AsyncReplyChannel<memoryModule.memory option>
    | Set of int * int * AsyncReplyChannel<memoryModule.memory option>
    | Get of int * AsyncReplyChannel<int option>

type memory = Mem of MailboxProcessor<message>

let inbox size (self: MailboxProcessor<message>) =
    let rec messageLoop (mem : memoryModule.memory) =
        async {
            let! msg = self.Receive()
            match msg with
            | Alloc (size, reply) ->
                let result = memoryModule.alloc size mem
                match result with
                | Some (newMem, ptr) ->
                    reply.Reply(Some (newMem, ptr))
                    return! messageLoop newMem
                | None ->
                    reply.Reply(None)
                    return! messageLoop mem
            | Free (ptr, size, reply) ->
                let result = memoryModule.free ptr size mem
                match result with
                | Some newMem ->
                    reply.Reply(Some newMem)
                    return! messageLoop newMem
                | None ->
                    reply.Reply(None)
                    return! messageLoop mem
            | Set (ptr, value, reply) ->
                let result = memoryModule.setMem ptr value mem
                match result with
                | Some newMem ->
                    reply.Reply(Some newMem)
                    return! messageLoop newMem
                | None ->
                    reply.Reply(None)
                    return! messageLoop mem
            | Get (ptr, reply) ->
                let result = memoryModule.getMem ptr mem
                reply.Reply(result)
                return! messageLoop mem
        }
    messageLoop (memoryModule.empty size)

let empty (size: int) : memory =
    Mem (MailboxProcessor.Start (inbox size))

let alloc (size: int) (Mem mbox) =
    mbox.PostAndReply(fun replyChannel -> Alloc(size, replyChannel))

let free (ptr: int) (size: int) (Mem mbox) =
    mbox.PostAndReply(fun replyChannel -> Free(ptr, size, replyChannel))

let setMem (ptr: int) (value: int) (Mem mbox) =
    mbox.PostAndReply(fun replyChannel -> Set(ptr, value, replyChannel))

let getMem (ptr: int) (Mem mbox) =
    mbox.PostAndReply(fun replyChannel -> Get(ptr, replyChannel))
