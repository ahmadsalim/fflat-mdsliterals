module FbCheck

open FbAst

let rec checkExpr expr =
  match expr with
  | Cst _  -> ()
  | Prim(_, e1, e2) -> ignore (checkExpr e1); ignore(checkExpr e2)
  | AdtConstr _  -> ()
  | Var _  -> ()
  | Lets(bs, eb) -> ignore (List.iter(fun (b, er) -> ignore (checkExpr er)) bs); ignore(checkExpr eb)
  | Letfuns(funs, eb) ->List.iter (fun (_,_,er) -> ignore(checkExpr er)) funs; ignore(checkExpr eb)
  | If(ec, et, ef) -> ignore(checkExpr ec); ignore(checkExpr et); ignore(checkExpr ef)
  | Fun(_, e) -> ignore(checkExpr e)
  | Call(ef, ep) -> ignore(checkExpr ef); ignore(checkExpr ep)
  | TplConstr(es) -> Array.iter(fun e -> ignore(checkExpr e)) es
  | Match(e, ps) ->
     ignore(checkExpr e);
     List.iter(fun (pat, grd, res) -> ignore(checkPattern pat Set.empty);
                                      ignore(checkExpr grd);
                                      ignore(checkExpr res)) ps
  | _ -> failwithf "Unsupported Expr: %A" expr
  expr
and checkPattern pat bound =
  let lBound' = ref bound
  match pat with
  | Cst _  -> ()
  | AdtConstr _  -> ()
  | Var name  ->
      if Set.contains name bound
        then failwithf "Non-linear binding of %s is not supported" name
        else lBound' := Set.add name !lBound'
  | Call(pf, pp) -> ignore(checkPattern pf bound); ignore(checkPattern pp bound)
  | TplConstr(ps) -> Array.iter(fun p -> ignore(checkPattern p bound)) ps
  | AsBinding(p, name) ->
      if Set.contains name bound
      then failwithf "Non-linear binding of %s is not supported" name
      else lBound' := Set.add name !lBound'; ignore(checkPattern p bound)
  | WildCard -> ()
  | _ -> failwithf "Unsupported Pattern: %A" pat
  (pat, !lBound')


let checkTypeReferences (decls : datadecl list) = 
    let socketCountMap = List.fold(fun m ((pms, name), _) -> Map.add name (List.length pms) m) Map.empty decls 
    let rec checkReference tn ctor =
        match tn with 
        | TypInt | TypBool | TypStr | TypVarSocket _ -> ()
        | TypVar _ -> failwith "Illegal typevar in constructor declaration"
        | TypTpl(tps) -> List.iter(fun tp -> checkReference tp ctor) tps
        | TypAdt(tps, nm) ->
            if Map.containsKey nm socketCountMap 
                then if List.length tps = Map.find nm socketCountMap 
                        then List.iter(fun tp -> checkReference tp ctor) tps
                        else failwithf "Datatype: %s does not take the given number of typevariables in constructor: %s" nm ctor
                else failwithf "Unknown datatype: %s referenced in constructor: %s" nm ctor
        | TypFun(t1, t2) -> checkReference t1 ctor; checkReference t2 ctor
    List.iter(fun (_, ctors) ->  
                List.iter(fun (ctor, fields, _) -> 
                    List.iter(fun (_, typ) -> checkReference typ ctor) fields
                ) ctors
        ) decls

let checkTypeParams (((pms, _), ctors) : datadecl) =
    let rec checkTypeParamUse tn ctor =
        match tn with
        | TypInt | TypBool | TypStr -> ()
        | TypVar _ -> failwith "Illegal typevar in constructor declaration"
        | TypVarSocket(pm) -> if not <| List.exists(fun pm' -> pm' = pm) pms 
                                   then failwithf "Unknown type parameter '%s used in constructor: %s" pm ctor
        | TypTpl(tps) | TypAdt(tps, _) -> List.iter(fun tp -> checkTypeParamUse tp ctor) tps
        | TypFun(t1, t2) -> checkTypeParamUse t1 ctor; checkTypeParamUse t2 ctor
    List.iter(fun (ctor, fields, _) -> 
                   List.iter(fun (_, typ) -> checkTypeParamUse typ ctor) fields ) ctors

let checkDecls (decls : datadecl list) =
    List.iter checkTypeParams decls
    checkTypeReferences decls

let rec checkStrdLit expr datatype =
   match expr with
   | Var _ | Cst _ | AdtConstr _  | WildCard  -> expr
   | AsBinding(e, n) -> AsBinding(checkStrdLit e datatype, n)
   | Call(AdtConstr n, e) -> Call(AdtConstr n, checkStrdLit e datatype)
   | TplConstr es -> TplConstr(Array.map (fun e -> checkStrdLit e datatype) es)
   | _ -> failwithf "Illegal expression returned by literal parser of type: %s" datatype