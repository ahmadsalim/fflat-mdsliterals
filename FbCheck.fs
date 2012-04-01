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


let rec checkStrdLit expr datatype =
   match expr with
   | Var _ | Cst _ | AdtConstr _  | WildCard  -> expr
   | AsBinding(e, n) -> AsBinding(checkStrdLit e datatype, n)
   | Call(AdtConstr n, e) -> Call(AdtConstr n, checkStrdLit e datatype)
   | TplConstr es -> TplConstr(Array.map (fun e -> checkStrdLit e datatype) es)
   | _ -> failwithf "Illegal expression returned by literal parser of type: %s" datatype