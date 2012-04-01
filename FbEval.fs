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
  | Adt of string * value array
  | Closure of string * expr * value lazyenv  (* (f, x, fBody, fDeclEnv) *)
  | AdtClosure of string                      (* constructor closure     *)

let rec getType (v : value) (types : typeenv) : typename =
    match v with
    | Val(basic) ->
        match basic with
        | Int _ -> TypInt
        | Str _ -> TypStr
        | Bool _-> TypBool
    | Tpl(vals)  ->
        TypTpl (List.map (fun vl -> getType vl types) (Array.toList vals))
    | Adt(name, _) ->
        TypAdt ( let (typ,_,_) = (Map.find name types) in typ )
    | AdtClosure(name) ->
      let (rettyp, args,_) = Map.find name types
      match args with
      | []             -> failwith "Unexpected ADT closure with no parameters"
      | [(_, typ)]     -> TypFun(typ, TypAdt(rettyp))
      | _              -> TypFun(TypTpl(args |> List.map (snd)), TypAdt(rettyp))
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
    | (TypAdt(nameTo), TypAdt(nameFrom)) when nameTo = nameFrom -> true
    | (TypFun(fTo, xTo), TypFun(fFrom, xFrom))
        when typeCastable fTo fFrom && typeCastable xTo xFrom -> true
    | _ -> false



let rec matchPattern (vl : value) (pat : expr) : value env option =
    match (vl, pat) with
    | (Val v1, Cst v2) ->
         if v1 = v2 then Some([]) else None
    | (Tpl vs, TplConstr ps) ->
         matchTuple vs ps
    | (Adt(nm1,[||]), AdtConstr(nm2)) ->
         if nm1 = nm2 then Some([]) else None
    | (Adt(nm1, [| v |]), Call(AdtConstr(nm2), p)) ->
         if nm1 = nm2 then matchPattern v p else None
    | (Adt(nm1, vs), Call(AdtConstr(nm2), ps)) ->
         if nm1 = nm2 then matchPattern (Tpl vs) ps else None
    | (_, AsBinding(p, nm)) ->
         let matched = matchPattern vl p
         match matched with
         | None -> None
         | Some(env) -> Some((nm, vl)::env)
    | (_, Var nm) -> Some([(nm, vl)])
    | (_, WildCard) -> Some([])
    | _ -> None
and matchTuple (vs : value array) (pats : expr array) : value env option =
    Array.zip vs pats
    |> Array.map (fun (vl, pat) -> matchPattern vl pat)
    |> Array.fold(fun res bnds -> match (res, bnds) with
                                  | (_, None) -> None
                                  | (None, _) -> None
                                  | (Some(rEnv), Some(bEnv)) -> Some(rEnv @ bEnv)) (Some [])


let rec eval (e : expr) (env : value env) (types : typeenv) : value =
    match e with
      | Cst v -> Val v
      | Var x  -> lookup env x
      | Prim(ope, e1, e2) ->
        let v1 = eval e1 env types
        match (ope, v1, e2) with
        | ("at", Adt(name, exprs), Var field) ->
                let findIndex fieldList fieldname =
                    let rec findIndex fieldList fieldname count =
                      match fieldList with
                      | (name, typ)::rest -> if name = fieldname
                                               then count
                                               else findIndex rest fieldname (count+1)
                      | []                -> -1
                    findIndex fieldList fieldname 0
                let i = findIndex (let (_, fieldnames, _) = (Map.find name types) in fieldnames) field
                if i >= 0 then exprs.[i] else failwithf "Constructor: %s does not have field: %s" name field

        | ("at", Tpl(exprs) , Cst(Int i))  ->
                if 0 < i && i <= exprs.Length
                    then exprs.[i-1]
                    else failwithf "Tuple does not contain any element at location %d" i
        | _  -> let v2 = eval e2 env types
                match (ope, v1, v2) with
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
                   | ("&&", Val(Bool b1), Val(Bool b2))-> Val(Bool(b1 && b2))
                   | ("||", Val(Bool b1), Val(Bool b2))-> Val(Bool(b1 || b2))
                   |  _ -> failwith "unknown primitive or wrong type"
      | Lets(bs, letBody) ->
        let rec bindValues bs acc =
            match bs with
            | [] -> acc
            | (x, eRhs)::rest ->
               let xVal = eval eRhs env types
               bindValues rest ((x, xVal)::acc)
        let letEnv = bindValues bs env
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
           | AdtClosure name ->
              let (_, args, guard) = Map.find name types
              let rec bindArguments args exprs =
                   List.fold2 (fun rest arg vl ->
                                if typeCastable (snd arg) (getType vl types)
                                   then (fst arg, vl) :: rest
                                   else failwithf "Uncompatible type %s for arg %s in constructor %s" ((snd arg).ToString()) (fst arg) (name)
                                   ) [] args exprs

              let argVal = eval eArg env types
              let vals   = match argVal with
                           | Tpl(vals)       -> Array.toList vals
                           | _               -> [ argVal ]
              let boundArgs = bindArguments args vals
              match eval guard boundArgs types with
              | Val(Bool true) -> Adt(name, List.map (snd) boundArgs |> List.rev |> List.toArray)
              | Val(Bool false)-> failwithf "Arguments given to constructor %s does not fulfill requirements" name
              | _              -> failwithf "Invalid guard on constructor %s" name
           | _ -> failwith "eval Call: not a function"
      | TplConstr exprs -> Tpl (Array.map (fun expr -> eval expr env types) exprs)
      | AdtConstr name ->
        let (_, args, guard) = Map.find name types
        match args with
        | [] -> Adt(name, [||])
        | _  -> AdtClosure(name)
      | Match(x, pats) ->
        let xVal = eval x env types
        let rec findPattern vl pats =
           match pats with
           | [] -> failwithf "Unable to pattern match value"
           | (pat, guard, eRes)::rest ->
             match matchPattern vl pat with
             | Some(env') ->
                match eval guard env' types with
                | Val(Bool true) -> eval eRes (env' @ env) types
                | Val(Bool false) -> findPattern vl rest
                | _ -> failwith "Invaild guard on pattern"
             | None -> findPattern vl rest
        findPattern xVal pats
      | StrdLit(typ, _) -> failwithf "unparsed structured data literal of type %s" typ
      | _               -> failwith  "unsupported expression"