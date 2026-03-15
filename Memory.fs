module Interpreter.Memory

type memory =
    { store : Map<int, int>
      next  : int }

let empty (memSize : int) : memory =
    { store = Map.empty
      next  = 0 }

let alloc (size : int) (mem : memory) : (memory * int) option =
    if size <= 0 then
        None
    else
        let firstAddr = mem.next
        let lastAddr  = firstAddr + size - 1

        let mutable newStore = mem.store
        for addr in firstAddr .. lastAddr do
            newStore <- Map.add addr 0 newStore

        let newMem =
            { store = newStore
              next  = mem.next + size }

        Some (newMem, firstAddr)

let free (ptr : int) (size : int) (mem : memory) : memory option =
    if size <= 0 then
        None
    else
        let firstAddr = ptr
        let lastAddr  = ptr + size - 1

        let mutable ok = true
        let mutable i = firstAddr
        while ok && i <= lastAddr do
            if Map.containsKey i mem.store then
                i <- i + 1
            else
                ok <- false

        if not ok then
            None
        else
            let mutable newStore = mem.store
            for addr in firstAddr .. lastAddr do
                newStore <- Map.remove addr newStore

            Some { mem with store = newStore }

let setMem (ptr : int) (value : int) (mem : memory) : memory option =
    if Map.containsKey ptr mem.store then
        let updatedStore = Map.add ptr value mem.store
        Some { mem with store = updatedStore }
    else
        None

let getMem (ptr : int) (mem : memory) : int option =
    Map.tryFind ptr mem.store