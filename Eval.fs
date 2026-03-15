module Interpreter.Eval

open Result
open Language
open State

let rec aexprEval (a: aexpr) (st: state) : int option =
    match a with
    | Num x -> Some x
    | Var v -> getVar v st
    | Add (b, c) ->
        match aexprEval b st with
        | Some x ->
            match aexprEval c st with
            | Some y -> Some (x + y)
            | None -> None
        | None -> None
    | Mul (b, c) ->
        match aexprEval b st with
        | Some x ->
            match aexprEval c st with
            | Some y -> Some (x * y)
            | None -> None
        | None -> None
    | Div (b, c) ->
        match aexprEval b st with
        | Some x ->
            match aexprEval c st with
            | Some y when y <> 0 -> Some (x / y)
            | _ -> None
        | None -> None
    | Mod (b, c) ->
        match aexprEval b st with
        | Some x ->
            match aexprEval c st with
            | Some y when y <> 0 -> Some (x % y)
            | _ -> None
        | None -> None

let rec aexprEval2 (a: aexpr) (st: state) : int option =
    match a with
    | Num n -> Some n
    | Var v -> getVar v st
    | Add(a1, a2) ->
        aexprEval2 a1 st
        |> Option.bind (fun b1 -> aexprEval2 a2 st |> Option.bind (fun b2 -> Some(b1 + b2)))
    | Mul(a1, a2) ->
        aexprEval2 a1 st
        |> Option.bind (fun b1 -> aexprEval2 a2 st |> Option.bind (fun b2 -> Some(b1 * b2)))
    | Div(a1, a2) ->
        aexprEval2 a1 st
        |> Option.bind (fun b1 ->
            aexprEval2 a2 st
            |> Option.bind (fun b2 -> if b2 <> 0 then Some(b1 / b2) else None))
    | Mod(a1, a2) ->
        aexprEval2 a1 st
        |> Option.bind (fun b1 ->
            aexprEval2 a2 st
            |> Option.bind (fun b2 -> if b2 <> 0 then Some(b1 % b2) else None))

let rec bexprEval (b: bexpr) (st: state) : bool option =
    match b with
    | TT -> Some true
    | Eq(a1, a2) ->
        aexprEval2 a1 st
        |> Option.bind (fun b1 -> aexprEval2 a2 st |> Option.bind (fun b2 -> Some(b1 = b2)))
    | Lt(a1, a2) ->
        aexprEval2 a1 st
        |> Option.bind (fun b1 -> aexprEval2 a2 st |> Option.bind (fun b2 -> Some(b1 < b2)))
    | Conj(b1, b2) ->
        bexprEval b1 st
        |> Option.bind (fun c1 -> bexprEval b2 st |> Option.bind (fun c2 -> Some(c1 && c2)))
    | Not b1 -> bexprEval b1 st |> Option.bind (fun c -> Some(not c))

let rec stmntEval (s: stmnt) (st: state) : state option =
    match s with
    | Skip -> Some st
    | Declare v -> declare v st
    | Assign(v, a) ->
        match aexprEval2 a st with
        | Some x -> setVar v x st
        | None -> None
    | Seq(s1, s2) ->
        match stmntEval s1 st with
        | Some st' -> stmntEval s2 st'
        | None -> None
    | If(guard, s1, s2) ->
        match bexprEval guard st with
        | Some true -> stmntEval s1 st
        | Some false -> stmntEval s2 st
        | None -> None
    | While(guard, s') ->
        match bexprEval guard st with
        | Some true ->
            match stmntEval s' st with
            | Some st' -> stmntEval (While(guard, s')) st'
            | None -> None
        | Some false -> Some st
        | None -> None
