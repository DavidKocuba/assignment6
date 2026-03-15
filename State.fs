module Interpreter.State

open Result
open Language
open Interpreter.Memory

let reservedVariableName (v: string) : bool =
    List.exists
        ((=) v)
        [ "if"
          "then"
          "else"
          "while"
          "declare"
          "print"
          "random"
          "fork"
          "__result__" ]

let validVariableName (v: string) : bool =
    if String.length v > 0 then
        let firstCharater = v.[0]
        let validLetter = System.Char.IsAsciiLetter firstCharater || firstCharater = '_'
        let valid = String.forall (fun x -> System.Char.IsAsciiLetterOrDigit x || x = '_') v
        valid && validLetter
    else
        false

type state =
    { variables : Map<string, int>
      memory    : memory }

let mkState (memSize : int) : state =
    { variables = Map.empty
      memory    = empty memSize }

let declare (x: string) (st: state) : state option =
    match Map.containsKey x st.variables with
    | true -> None
    | false ->
        if validVariableName x && not (reservedVariableName x) then
            let newVars = Map.add x 0 st.variables
            Some { st with variables = newVars }
        else
            None

let getVar (x: string) (st: state) : int option =
    Map.tryFind x st.variables

let setVar (x: string) (v: int) (st: state) : state option =
    match Map.containsKey x st.variables with
    | true ->
        let newVars = Map.add x v st.variables
        Some { st with variables = newVars }
    | false ->
        None

let alloc (x : string) (size : int) (st : state) : state option =
    match Interpreter.Memory.alloc size st.memory with
    | None ->
        None
    | Some (mem', ptr) ->
        match setVar x ptr st with
        | None -> None
        | Some stWithVar ->
            Some { stWithVar with memory = mem' }

let free (ptr : int) (size : int) (st : state) : state option =
    match Interpreter.Memory.free ptr size st.memory with
    | None -> None
    | Some mem' -> Some { st with memory = mem' }

let getMem (ptr : int) (st : state) : int option =
    Interpreter.Memory.getMem ptr st.memory

let setMem (ptr : int) (value : int) (st : state) : state option =
    match Interpreter.Memory.setMem ptr value st.memory with
    | None -> None
    | Some mem' -> Some { st with memory = mem' }
