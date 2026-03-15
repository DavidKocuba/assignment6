module Interpreter.State

open Result
open Language

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

type state = { variables: Map<string, int> }

let mkState () : state = { variables = Map.empty }

let declare (x: string) (st: state) : state option =
    match Map.containsKey x st.variables with
    | true -> None
    | false ->
        if validVariableName x && not (reservedVariableName x) then
            Some { variables = Map.add x 0 st.variables }
        else
            None

let getVar (x: string) (st: state) : int option = Map.tryFind x st.variables

let setVar (x: string) (v: int) (st: state) : state option =
    match Map.containsKey x st.variables with
    | true -> Some { variables = Map.add x v st.variables }
    | false -> None

let push _ = failwith "not implemented"
let pop _ = failwith "not implemented"
