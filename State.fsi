module Interpreter.State

open Language

type state = { variables: Map<string, int> }

val mkState: unit -> state
val declare: string -> state -> state option
val getVar: string -> state -> int option
val setVar: string -> int -> state -> state option
