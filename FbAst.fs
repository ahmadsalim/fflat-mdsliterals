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
        | Int i1, Int i2            -> i1 = i2
        | Bool b1, Bool b2          -> b1 = b2
        | Str s1, Str s2            -> s1 = s2
        | _                         -> failwith "equality performed to incompatible types"
    | _ -> false

  override this.GetHashCode() =
    match this with
    | Bool b    ->   b.GetHashCode()
    | Int i     ->   i.GetHashCode()
    | Str s     ->   s.GetHashCode()

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
  | StrdLit of string * string                         (* (literalType, literalValue)  *)
  | Var of string
  | Lets of (string * expr) list * expr
  | Prim of string * expr * expr
  | If of expr * expr * expr
  | Letfuns of (string * string * expr) list * expr    (* (f, x, fBody, letBody)       *)
  | Fun of string * expr
  | Call of expr * expr
  | TplConstr of expr array
  | AdtConstr of string                                (* (constructor name)           *)
  | Match of expr * (expr * expr * expr) list          (* (expr, [(pat, guard, res)...]*)
  | AsBinding of expr * string                         (* as pattern                   *)
  | WildCard
  | Quote of string


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

let mkTplConstr exprs =
    match exprs with
    | [] | _::[] -> failwith "Illegal tuple with one argument or less"
    | _     -> TplConstr(List.toArray exprs)

let mkList exprs =
    List.fold(fun rest expr -> Call(AdtConstr("@Cons"), TplConstr([| expr; rest |]))) (AdtConstr("@Nil")) (List.rev exprs)



(* A type is int, bool, function, or type variable: *)

type typename =
     | TypInt                              (* integers                   *)
     | TypBool                             (* booleans                   *)
     | TypStr                              (* strings                    *)
     | TypTpl of typename list             (* tuples                     *)
     | TypAdt of typename list * string    (* adt of types               *)
     | TypFun of typename * typename       (* (argumenttype, resulttype) *)
     | TypVar of typevar                   (* type variable              *)
     | TypVarSocket of string              (* represents a type variable at a data declaration *)

and tyvarkind =
     | NoLink of string                    (* uninstantiated type var.   *)
     | LinkTo of typename                  (* instantiated to typ        *)

and typevar =
     (tyvarkind * int) ref                 (* kind and binding level     *)

(* ADT declaration types *)
type constrdecl = string * (string * typename) list * expr     (* (contructor name, datatypes, guard) *)
type datadecl = (string list * string) * constrdecl list       (* ((type vars, type name), constructors) *)
type typeenv = Map<string, (string list * string) * (string * typename) list * expr>

(* ADT Helper functions  *)
let mkTypTpl typs =
  match typs with
  | [] -> failwithf "Unknown type list with no arguments"
  | [x] -> x
  | _ -> TypTpl typs

let addDataDeclToEnv (decl : datadecl) (env:typeenv) : typeenv =
    let ((tvs, typname), constrsdecl) = decl
    if env |> Map.fold (fun s k v -> Set.add v s) Set.empty |> Set.exists (fun ((_,t),_,_) -> t = typname)
      then failwithf "type %s is already declared" typname
    List.fold (fun env (constrname, elements, guard) ->
        if not (Map.containsKey constrname env)
        then Map.add constrname ((tvs, typname), elements, guard) env
        else failwithf "Constructor %s is already defined" constrname) env constrsdecl

let addTypeVarToList tvar tvars =
    if List.exists (fun (name, typ) -> name = (fst tvar)) tvars
    then failwithf "%s is already bound in constructor" (fst tvar)
    else tvar::tvars

let defaultAdts : datadecl list =
      [((["a"],"list"),        [("@Nil",  [],                                                                           Cst(Bool true));
                                ("@Cons", [("head", TypVarSocket("a")); ("tail", TypAdt([TypVarSocket("a")], "list"))], Cst(Bool true))]);
       ((["a";"b"], "either"), [("Left",  [("value", TypVarSocket("a"))],                                               Cst(Bool true));
                                ("Right", [("value", TypVarSocket("b"))],                                               Cst(Bool true))])]
