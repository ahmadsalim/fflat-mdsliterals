(*
    Abstract syntax for the Fb programming language
    Original Author: Peter Sestoft <sestoft@itu.dk> (for the the microML programming language)
*)

module FbType

open FbAst

(* Environment operations *)

type 'v env = (string * 'v) list

let rec lookup env x =
    match env with
      | []        -> failwith (x + " not found")
      | (y, v)::r -> if x=y then v else lookup r x;;

(* Operations on sets of type variables, represented as lists.
   Inefficient but simple.  Basically compares type variables
   on their string names.  Correct so long as all type variable names
   are distinct. *)

let rec mem x vs =
    match vs with
    | []      -> false
    | v :: vr -> x=v || mem x vr;;

(* union(xs, ys) is the set of all elements in xs or ys, without duplicates *)

let rec union (xs, ys) =
    match xs with
      | []    -> ys
      | x::xr -> if mem x ys then union(xr, ys)
                 else x :: union(xr, ys);;

(* unique xs  is the set of members of xs, without duplicates *)

let rec unique xs =
    match xs with
    | []    -> []
    | x::xr -> if mem x xr then unique xr else x :: unique xr;;


(* A type scheme is a list of generalized type variables, and a type: *)

type typescheme =
     | TypeScheme of typevar list * typename   (* type variables and type    *)

(* *)

let setTvKind tyvar newKind =
    let (kind, lvl) = !tyvar
    in tyvar := (newKind, lvl)

let setTvLevel tyvar newLevel =
    let (kind, lvl) = !tyvar
    in tyvar := (kind, newLevel)

(* Normalize a type; make type variable point directly to the
   associated type (if any).  This is the `find' operation, with path
   compression, in the union-find algorithm. *)

let rec normType t0 =
    match t0 with
      | TypVar tyvar ->
        match !tyvar with
          | (LinkTo t1, _) -> let t2 = normType t1
                              in setTvKind tyvar (LinkTo t2); t2
          | _ -> t0
      |  _ -> t0;

let rec freeTypeVars t : typevar list =
    match normType t with
    | TypInt        -> []
    | TypBool       -> []
    | TypStr        -> []
    | TypVar tv     -> [tv]
    | TypFun(t1,t2) -> union(freeTypeVars t1, freeTypeVars t2)
    | TypTpl(typs)  -> List.fold(fun freeVars t -> union(freeVars, freeTypeVars t)) [] typs
    | TypAdt(typs, name) -> List.fold(fun freeVars t -> union(freeVars, freeTypeVars t)) [] typs
    | TypVarSocket _ -> failwith "freeTypeVars is not supported for type var socket" 


let occurCheck tyvar tyvars =
    if mem tyvar tyvars then failwith "type error: circularity" else ()

let pruneLevel maxLevel tvs =
    let reducelevel tyvar =
        let (_, level) = !tyvar
        in setTvLevel tyvar (min level maxLevel)
    in List.iter reducelevel tvs

(* Make type variable tyvar equal to type t (by making tyvar link to t),
   but first check that tyvar does not occur in t, and reduce the level
   of all type variables in t to that of tyvar.  This is the `union'
   operation in the union-find algorithm.  *)

let rec linkVarToType tyvar t =
    let (_, level) = !tyvar
    let fvs = freeTypeVars t
    in
        occurCheck tyvar fvs;
        pruneLevel level fvs;
        setTvKind tyvar (LinkTo t)

let rec typeToString t : string =
    match t with
      | TypInt                     -> "int"
      | TypBool                    -> "bool"
      | TypStr                     -> "string"
      | TypAdt(ts, name)           -> List.fold (fun s t -> typeToString t + " " + s) "" ts + name
      | TypVar _ | TypVarSocket _  -> failwith "typeToString impossible"
      | TypFun(t1, t2)             -> "function"
      | TypTpl _                   -> "tuple"

(* Unify two types, equating type variables with types as necessary *)

let rec unify t1 t2 : unit =
    let t1' = normType t1
    let t2' = normType t2
    in match (t1', t2') with
       | (TypInt, TypInt) -> ()
       | (TypBool, TypBool) -> ()
       | (TypStr, TypStr) -> ()
       | (TypFun(t11, t12), TypFun(t21, t22)) -> (unify t11 t21; unify t12 t22)
       | (TypTpl(ts1), TypTpl(ts2)) ->
              List.iter(fun (t1, t2) -> unify t1 t2) <| List.zip ts1 ts2
       | (TypAdt(ts1, nm1), TypAdt(ts2, nm2)) when nm1 = nm2 ->
              List.iter(fun (t1, t2) -> unify t1 t2) <| List.zip ts1 ts2
       | (TypVar tv1, TypVar tv2) ->
         let (_, tv1level) = !tv1
         let (_, tv2level) = !tv2
         in if tv1 = tv2                then ()
            else if tv1level < tv2level then linkVarToType tv1 t2'
                                        else linkVarToType tv2 t1'
       | (TypVar tv1, _       ) -> linkVarToType tv1 t2'
       | (_,        TypVar tv2) -> linkVarToType tv2 t1'
       | (TypInt,     t)  -> failwith ("type error: int and " + typeToString t)
       | (TypBool,     t) -> failwith ("type error: bool and " + typeToString t)
       | (TypStr,      t) -> failwith ("type error: string and " + typeToString t)
       | (TypAdt(_, name), t)-> failwith (sprintf("type error: %s and ") name + typeToString t)
       | (TypTpl _, t)     -> failwith ("type error: tuple and " + typeToString t)
       | (TypFun _,   t)  -> failwith ("type error: function and " + typeToString t)
       | (TypVarSocket _, t) -> failwith ("type error: cannot unify type var socket")

(* Generate fresh type variables *)

let tyvarno = ref 0

let newTypeVar level : typevar =
    let rec mkname i res =
            if i < 26 then char(97+i) :: res
            else mkname (i/26-1) (char(97+i%26) :: res)
    let intToName i = new System.String(Array.ofList('\'' :: mkname i []))
    in (tyvarno := !tyvarno + 1;
        ref (NoLink (intToName (!tyvarno)), level))

(* Generalize over type variables not free in the context; that is,
   over those whose level is higher than the current level: *)

let rec generalize level (t : typename) : typescheme =
    let notfreeincontext tyvar =
        let (_, linkLevel) = !tyvar
        in  linkLevel > level
    let tvs = List.filter notfreeincontext (freeTypeVars t)
    in TypeScheme(unique tvs, t)  // The unique call seems unnecessary because freeTypeVars has not duplicates??

(* Copy a type, replacing bound type variables as dictated by tvenv,
   and non-bound ones by a copy of the type linked to *)

let rec copyType subst t : typename =
    match t with
    | TypVar tyvar ->
      let (* Could this be rewritten so that loop does only the substitution *)
          rec loop subst1 =
          match subst1 with
           | (tyvar1, type1) :: rest -> if tyvar1 = tyvar then type1 else loop rest
           | [] -> match !tyvar with
                     | (NoLink _, _)  -> t
                     | (LinkTo t1, _) -> copyType subst t1
      in loop subst
    | TypFun(t1,t2) -> TypFun(copyType subst t1, copyType subst t2)
    | TypInt        -> TypInt
    | TypBool       -> TypBool
    | TypStr        -> TypStr
    | TypTpl(ts)    -> TypTpl(List.map(copyType subst) ts)
    | TypAdt(tvs, n)-> TypAdt(tvs, n)
    | TypVarSocket _ -> failwith "copying type variable sockets is not possible"


(* Create a type from a type scheme (tvs, t) by instantiating all the
   type scheme's parameters tvs with fresh type variables *)

let specialize level (TypeScheme(tvs, t)) : typename =
    let bindfresh tv = (tv, TypVar(newTypeVar level))
    in match tvs with
       | [] -> t
       | _  -> let subst = List.map bindfresh tvs
               in copyType subst t

(* Pretty-print type, using names 'a, 'b, ... for type variables *)

let rec showType t : string =
    let rec pr t showTplParen showFunParen =
        match normType t with
        | TypInt            -> "int"
        | TypBool           -> "bool"
        | TypStr            -> "string"
        | TypAdt(tvs, name) -> 
            match tvs with
            | [] -> name
            | _  -> sprintf "%s<%s>" name (System.String.Join(",", List.map (fun tv -> pr tv false false) tvs))
        | TypTpl(ts)   ->
             let innerTs = (List.map(fun t -> pr t true true) ts
                              |> List.reduce (fun str t -> sprintf "%s * %s" str t))
             if showTplParen then sprintf "(%s)" innerTs else innerTs
        | TypVar tyvar ->
          match !tyvar with
            | (NoLink name, _) -> name
            | _                -> failwith "showType impossible"
        | TypFun(t1, t2) -> 
            let innerT1 = pr t1 false true
            let innerT2 = pr t2 false false
            if showFunParen then sprintf "(%s -> %s)" innerT1 innerT2 else sprintf "%s -> %s" innerT1 innerT2
        | TypVarSocket _ -> failwith "showType cannot be used on type variable sockets"
    in pr t false false

(* A type environment maps a program variable name to a typescheme *)

type tenv = typescheme env

(* Type inference: tyinf e0 returns the type of e0, if any *)

let replaceSockets typ lvl =
  let mappedSockets = ref Map.empty<string, typename>
  let rec replace typ =
    match typ with
    | TypInt | TypStr | TypBool | TypVar _ -> typ
    | TypFun(targs, tret) -> TypFun(replace targs, replace tret)
    | TypTpl(ts) -> TypTpl(List.map replace ts)
    | TypAdt(tvs, nm) -> TypAdt(List.map replace tvs, nm)
    | TypVarSocket(tparam) -> if Map.containsKey tparam !mappedSockets
                                    then Map.find tparam !mappedSockets
                                    else let tvar = TypVar(newTypeVar lvl)
                                         mappedSockets := Map.add tparam tvar !mappedSockets;
                                         tvar
  replace typ

let rec tyinf e0 (types : typeenv) =
    (* (typ lvl env e) returns the type of e in env at level lvl *)
    let rec typ (lvl : int) (env : tenv) (e : expr) : typename =
        match e with
        | Cst(Int i) -> TypInt
        | Cst(Bool b) -> TypBool
        | Cst(Str s) -> TypStr
        | Var x  -> specialize lvl (lookup env x)
        | Prim(ope, e1, e2) ->
          let t1 = typ lvl env e1
          match (ope, t1, e2) with
          | ("at", TypAdt(_, name), Var field) ->
              failwith "type inference for adts is not yet supported"
          | ("at", TypTpl(typs), Cst(Int i)) ->
              if 0 < i && i <= typs.Length
                    then List.nth typs (i-1)
                    else failwithf "Tuple does not contain any element at location %d" i
          | _ ->
             let t2 = typ lvl env e2
             match ope with
                | "*" -> (unify TypInt t1; unify TypInt t2; TypInt)
                | "+" -> (unify TypInt t1; unify TypInt t2; TypInt)
                | "-" -> (unify TypInt t1; unify TypInt t2; TypInt)
                | "/" -> (unify TypInt t1; unify TypInt t2; TypInt)
                | "=" -> (unify t1 t2; TypBool)
                | "<>"-> (unify t1 t2; TypBool)
                | "<" -> (unify TypInt t1; unify TypInt t2; TypBool)
                | "<="-> (unify TypInt t1; unify TypInt t2; TypBool)
                | ">" -> (unify TypInt t1; unify TypInt t2; TypBool)
                | ">="-> (unify TypInt t1; unify TypInt t2; TypBool)
                | "^" -> (unify TypStr t1; unify TypStr t2; TypStr)
                | "&&"-> (unify TypBool t1; unify TypBool t2; TypBool)
                | "||"-> (unify TypBool t1; unify TypBool t2; TypBool)
                | _   -> failwith ("unknown primitive " + ope)
        | Lets(vars, letBody) ->
          let lvl1 = lvl + 1
          let letEnv =
             List.fold(fun env (x, eRhs) ->
                         let resTy = typ lvl1 env eRhs
                         (x, generalize lvl resTy) :: env) env vars
          typ lvl letEnv letBody
        | If(e1, e2, e3) ->
          let t2 = typ lvl env e2
          let t3 = typ lvl env e3
          unify TypBool (typ lvl env e1);
          unify t2 t3;
          t2
        | Letfuns(funs, letBody) ->
          let lvl1 = lvl + 1
          let bodyEnv =
             let funTyps =
                   List.map(fun (f, x, _) -> (f, TypeScheme([], TypVar(newTypeVar lvl1)))) funs
             List.fold(fun env (f, x, fBody) ->
                let fTyp =
                   let rec getFTyp typs =
                     match typs with
                     | [] -> failwithf "Error when resolving types for letfuns"
                     | (fn, TypeScheme(_, fTyp))::typs ->
                         if fn = f then fTyp else getFTyp typs
                   getFTyp funTyps
                let xTyp = TypVar(newTypeVar lvl1)
                let fBodyEnv = (x, TypeScheme([], xTyp))
                             :: (funTyps @ env)
                let rTyp = typ lvl1 fBodyEnv fBody
                let _    = unify fTyp (TypFun(xTyp, rTyp))
                (f, generalize lvl fTyp) :: env) env funs
          typ lvl bodyEnv letBody
        | Fun(x, body) ->
           let xTyp = TypVar(newTypeVar lvl)
           let bodyEnv =  (x, TypeScheme([], xTyp))::env
           TypFun(xTyp, typ lvl bodyEnv body)
        | Call(eFun, eArg) ->
          let tf = typ lvl env eFun
          let tx = typ lvl env eArg
          let tr = TypVar(newTypeVar lvl)
          unify tf (TypFun(tx, tr));
          tr
        | TplConstr(exprs) ->
          let ts = List.map(typ lvl env) (Array.toList exprs)
          TypTpl ts
        | Match(expr, cases) ->
            let te = typ lvl env expr
            let tr = TypVar(newTypeVar lvl)
            List.iter(fun (pat, guard, ret) -> 
                let (tp, pEnv) = typPat lvl pat;
                let env' = pEnv @ env
                unify te tp;
                unify TypBool (typ lvl env' guard);
                unify tr (typ lvl env' ret)
                ) cases ;
            tr
        | AdtConstr(nm) ->
            let ((tvs, tn), args, guard) = Map.find nm types
            let stvs = List.map TypVarSocket tvs
            let tcret = TypAdt(stvs, tn)
            let tconstr = 
                match args with
                | []        -> tcret
                | [nm, tp]  -> TypFun(tp, tcret)
                | _         -> TypFun(TypTpl (List.map snd args), tcret)
            replaceSockets tconstr lvl
        | StrdLit(typ, _) -> failwithf "unparsed structured data literal of type %s" typ
        | _ -> failwithf "type inference is not yet supported for expr: %A" e
    and typPat (lvl : int) (pat : expr) : typename * (string * typescheme) list =
        match pat with
        | Cst(Int i)  -> (TypInt, [])
        | Cst(Str s)  -> (TypStr, [])
        | Cst(Bool b) -> (TypBool, [])
        | Var x -> let tv = TypVar(newTypeVar lvl) in (tv, [(x, generalize lvl tv)])
        | WildCard -> (TypVar(newTypeVar lvl), [])
        | AsBinding(p, nm) -> let (tp, pEnv) = typPat lvl p in (tp, (nm, generalize lvl tp)::pEnv)
        | TplConstr(ps) ->
            let patTypRes = Array.map(fun p -> typPat lvl p) ps |> List.ofArray
            (TypTpl(List.map fst patTypRes), List.fold (@) [] (List.map snd patTypRes))
        | AdtConstr(nm) ->
             let ((tvs, tn), _, _) = Map.find nm types
             let stvs = List.map TypVarSocket tvs
             let t = replaceSockets <| TypAdt(stvs, tn) <| lvl
             (t, [])
        | Call(AdtConstr(nm), gargs) ->
            let ((tvs, tn), args, guard) = Map.find nm types
            let stvs = List.map TypVarSocket tvs
            let tcret = TypAdt(stvs, tn)
            let tconstr = 
                match args with
                | []        -> tcret
                | [nm, tp]  -> TypFun(tp, tcret)
                | _         -> TypFun(TypTpl (List.map snd args), tcret)
            let tconstr = replaceSockets tconstr lvl
            match (tconstr, gargs) with
            | (TypFun(TypTpl(targs), tr), TplConstr(ps)) -> 
                let patTypRes = Array.map(fun p -> typPat lvl p) ps |> List.ofArray
                let (targs', pEnv) = (List.map fst patTypRes, List.fold (@) [] (List.map snd patTypRes))
                List.iter (fun (t1, t2) -> unify t1 t2) (List.zip targs targs');
                (tr, pEnv)
            | (TypFun(targ, tr), p) ->
                let (targ', pEnv) = typPat lvl p;
                unify targ targ';
                (tr, pEnv)
            | _ -> failwith "Internal error: unknown result when replaced sockets in pattern match type inference"
        | _ -> failwith "Illegal pattern in type inference"
    typ 0 [] e0

let inferType e types =
    (tyvarno := 0;
     showType (tyinf e types));;
