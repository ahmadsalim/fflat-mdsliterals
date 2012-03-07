 (*
    Evaluator for the Fb programming language
    Original Author: Peter Sestoft <sestoft@itu.dk> (for the the microML programming language)
 *)

module FbEval

#nowarn "40" //Disable warning on recursive object definition used in knot-tying 
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
  | Tpl of value array
  | Ast of string * value array
  | Closure of string * expr * value lazyenv  (* (f, x, fBody, fDeclEnv) *)

let rec getType (v : value) (types : typeenv) : typename =
    match v with
    | Val(basic) -> 
        match basic with
        | Int _ -> TypInt
        | Str _ -> TypStr
        | Bool _-> TypBool
    | Tpl(vals)  ->
        TypTpl (List.map (fun vl -> getType vl types) (Array.toList vals)) 
    | Ast(name, _) ->
        TypAst ( let (typ,_,_) = (Map.find name types) in typ )
    | Closure _    ->
        TypFun(TypDyn,TypDyn) //Only dynamic functions are supported as there is no typechecking
 
let rec typeCastable (typTo : typename) (typFrom : typename) : bool = 
    match (typTo, typFrom) with
    | (TypDyn,  _) -> true
    | (TypInt,  TypInt) -> true
    | (TypStr,  TypStr) -> true
    | (TypBool, TypBool) -> true
    | (TypTpl(valsTo), TypTpl(valsFrom)) 
            when List.forall2 (fun vT vF -> typeCastable vT vF) valsTo valsFrom -> true
    | (TypAst(nameTo), TypAst(nameFrom)) when nameTo = nameFrom -> true
    | (TypFun(fTo, xTo), TypFun(fFrom, xFrom)) 
        when typeCastable fTo fFrom && typeCastable xTo xFrom -> true
    | _ -> false
                    

let rec eval (e : expr) (env : value env) (types : typeenv) : value =
    match e with
      | Cst v -> Val v
      | Var x  -> lookup env x
      | Prim(ope, e1, e2) -> 
        let v1 = eval e1 env types
        let v2 = eval e2 env types
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
           | ("at", Tpl(exprs) , Val(Int i))  -> 
                if 0 < i && i <= exprs.Length 
                    then exprs.[i-1] 
                    else failwithf "Tuple does not contain any element at location %d" i  
           | ("&&", Val(Bool b1), Val(Bool b2))-> Val(Bool(b1 && b2))
           | ("||", Val(Bool b1), Val(Bool b2))-> Val(Bool(b1 || b2))
           |  _ -> failwith "unknown primitive or wrong type"
      | Let(x, eRhs, letBody) -> 
        let xVal = eval eRhs env types
        let letEnv = (x, xVal) :: env 
        eval letBody letEnv types
      | If(e1, e2, e3) -> 
        match eval e1 env types with
          | Val(Bool(true))   -> eval e2 env types
          | Val(Bool(false))  -> eval e3 env types
          | _  -> failwith "evaluation condition in if statement did not return a boolean value"
      | Letfuns(funs, letBody) -> 
        let rec funEnv = LazyList.map (fun (f, x, fBody) -> 
            (f, Closure(x, fBody, LazyList.append funEnv 
                                     <| LazyList.ofList env))) <| LazyList.ofList funs
        eval letBody <| (LazyList.toList funEnv) @ env <| types
      | Fun(x, body) ->
        Closure(x, body, LazyList.ofList env)
      | Call(eFun, eArg) -> 
        let fClosure = eval eFun env types
        match fClosure with
           | Closure (x, fBody, fDeclEnv) ->
             let xVal = eval eArg env types
             let fBodyEnv = (x, xVal)::(LazyList.toList fDeclEnv)
             eval fBody fBodyEnv types
           | _ -> failwith "eval Call: not a function"
      | TplConstr exprs -> Tpl (Array.map (fun expr -> eval expr env types) exprs)
      | AstConstr(name, exprs) -> 
           let (_, args, guard) = Map.find name types
           let rec bindArguments args exprs = 
                List.fold2 (fun rest arg expr ->
                             let evaled = eval expr env types
                             if typeCastable (snd arg) (getType evaled types) 
                                then (fst arg, evaled) :: rest
                                else failwithf "Uncompatible type %s for arg %s in constructor %s" ((snd arg).ToString()) (fst arg) (name)
                                ) [] args exprs
           let boundArgs = bindArguments args (Array.toList exprs)
           match eval guard boundArgs types with
           | Val(Bool true) -> Ast(name, List.map (snd) boundArgs |> List.rev |> List.toArray)
           | Val(Bool false)-> failwithf "Arguments given to constructor %s does not fulfill requirements" name
           | _              -> failwithf "Invalid guard on constructor %s" name
                               

      | StrdLit(typ, _) -> failwithf "unparsed structured data literal of type %s" typ