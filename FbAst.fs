(*
    Abstract syntax for the Fb programming language 
    Original Author: Peter Sestoft <sestoft@itu.dk> (for the the microML programming language) 
*)

module FbAst
open System

[<CustomEquality>]
[<CustomComparison>]
type valtype =
  | Int of int
  | Bool of bool
  | Str of string


  override this.Equals v =
    match v with
    | :? valtype as v' -> 
        match this, v' with
        | Int i1, Int i2    -> i1 = i2
        | Bool b1, Bool b2  -> b1 = b2
        | Str s1, Str s2    -> s1 = s2
        | _                 -> failwith "equality performed to incompatible types"
    | _ -> false

  override this.GetHashCode() =
    match this with
    | Bool b -> b.GetHashCode()
    | Int i  -> i.GetHashCode()
    | Str s  -> s.GetHashCode()

  interface IComparable with
    member this.CompareTo v =
        match v with
        | :? valtype as v' ->
            match this, v' with
            | Int i1, Int i2   -> i1.CompareTo(i2)
            | Str s1, Str s2   -> s1.CompareTo(s2)
            | _                -> failwith "comparison performed to incompatible types"
        | _ -> raise (ArgumentException())


type expr = 
  | Cst of valtype
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr
  | If of expr * expr * expr
  | Letfuns of (string * string * expr) list * expr    (* (f, x, fBody, letBody) *)
  | Fun of string * expr
  | Call of expr * expr

(** Smart constuctors for expr **)

let mkLetfun name args fBody =
    match args with
    | []        -> failwith "Illegal function with no arguments"
    | arg::argr -> 
        let innerFun = List.foldBack (fun a f -> Fun(a, f)) argr fBody
        (name, arg, innerFun)

let mkFun args fBody =
    match args with
    | []        -> failwith "Illegal function with no arguments"
    | arg::argr -> List.foldBack (fun a f -> Fun(a, f)) args fBody
    