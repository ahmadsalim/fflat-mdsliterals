 (*
    Evaluator for the Fb programming language
    Original Author: Peter Sestoft <sestoft@itu.dk> (for the the microML programming language)
 *)

module FbEval

open FbAst

(* Environment operations *)

type 'v env = (string * 'v) list
type 'v lazyenv = (string * 'v) LazyList

let rec lookup env x =
    match env with 
      | []        -> failwith (x + " not found")
      | (y, v)::r -> if x=y then v else lookup r x;;

(* A runtime value is an integer or a function closure *)

type value = 
  | Val of valtype
  | Closure of string * string * expr * value lazyenv  (* (f, x, fBody, fDeclEnv) *)
  | AnonymousClosure of string * expr * value env      (* (x, body, declEnv)      *)

let rec eval (e : expr) (env : value env) : value =
    match e with
      | Cst v -> Val v
      | Var x  -> lookup env x
      | Prim(ope, e1, e2) -> 
        let v1 = eval e1 env
        let v2 = eval e2 env
        in match (ope, v1, v2) with
           | ("*",  Val(Int i1), Val(Int i2)) -> Val(Int(i1 * i2))
           | ("+",  Val(Int i1), Val(Int i2)) -> Val(Int(i1 + i2))
           | ("-",  Val(Int i1), Val(Int i2)) -> Val(Int(i1 - i2))
           | ("/",  Val(Int i1), Val(Int i2)) -> Val(Int(i1 / i2))
           | ("=",  Val(v1)    , Val(v2))     -> Val(Bool(v1 = v2))
           | ("<>", Val(v1)    , Val(v2))     -> Val(Bool(v1 <> v2))
           | ("<",  Val(v1)    , Val(v2))     -> Val(Bool(v1 < v2))
           | ("<=", Val(v1)    , Val(v2))     -> Val(Bool(v1 <= v2))
           | (">" , Val(v1)    , Val(v2))     -> Val(Bool(v1 > v2))
           | (">=", Val(v1)    , Val(v2))     -> Val(Bool(v1 >= v2))
           | ("^",  Val(Str s1), Val(Str s2)) -> Val(Str(s1 + s2))
           |  _ -> failwith "unknown primitive or wrong type"
      | Let(x, eRhs, letBody) -> 
        let xVal = eval eRhs env
        let letEnv = (x, xVal) :: env 
        eval letBody letEnv
      | If(e1, e2, e3) -> 
        match eval e1 env with
          | Val(Bool(true))   -> eval e2 env
          | Val(Bool(false))  -> eval e3 env
          | _  -> failwith "evaluation condition in if statement did not return a boolean value"
      | Letfuns(funs, letBody) -> 
        let rec funEnv = LazyList.map (fun (f, x, fBody) -> 
            (f, Closure(f, x, fBody, LazyList.append funEnv 
                                     <| LazyList.ofList env))) <| LazyList.ofList funs
        eval letBody <| (LazyList.toList funEnv) @ env
      | Fun(x, body) ->
        AnonymousClosure(x, body, env)
      | Call(eFun, eArg) -> 
        let fClosure = eval eFun env
        match fClosure with
           | Closure (f, x, fBody, fDeclEnv) ->
             let xVal = eval eArg env
             let fBodyEnv = (x, xVal) :: (f, fClosure) :: (LazyList.toList fDeclEnv)
             eval fBody fBodyEnv
           | AnonymousClosure(x, body, declEnv) ->
             let xVal = eval eArg env
             eval body <| (x, xVal)::declEnv
           | _ -> failwith "eval Call: not a function";;