module JsonLiteralParser

open FbAst
open FbInterpret

type JsonLiteralParser() = 
    class
        interface IStructuredDataLiteralParser with
            member this.HandledDataType with get() = "json"
            member this.ResultDataTypes 
                with get() =
                    seq {
                            yield (([], "json_pair"),      [("JsPair",     [("key", TypStr); ("value", TypAdt([], "json"))],         Cst(Bool true))]);
                            yield (([], "json"),           [("JsObject",   [("members", TypAdt([TypAdt([], "json_pair")], "list"))], Cst(Bool true));
                                                            ("JsArray",    [("elements",TypAdt([TypAdt([], "json")], "list"))],      Cst(Bool true));
                                                            ("JsString",   [("value", TypStr)],                                      Cst(Bool true));
                                                            ("JsInteger",  [("value", TypInt)],                                      Cst(Bool true));
                                                            ("JsBoolean",  [("value", TypBool)],                                     Cst(Bool true))
                                                            ("JsNull",     [],                                                       Cst(Bool true))])
                        }
            member this.ParseLiteral parser str =
                let rec replaceQuotes expr = 
                    match expr with
                    | Quote s -> parser(s)
                    | Cst _ | Var _ | WildCard | AdtConstr _ -> expr
                    | Lets(bindings, body) -> Lets(List.map (fun (nm, e) -> (nm, replaceQuotes e)) bindings, replaceQuotes body)
                    | Prim(opr, e1, e2) -> Prim(opr, replaceQuotes e1, replaceQuotes e2)
                    | If(ec, et, ef) -> If(replaceQuotes ec, replaceQuotes et, replaceQuotes ef)
                    | Letfuns(bindings, body) -> Letfuns(List.map (fun (fn, an, e) -> (fn, an, replaceQuotes e)) bindings, replaceQuotes body)
                    | Fun(argname, body) -> Fun(argname, replaceQuotes body)
                    | Call(func, arg) -> Call(replaceQuotes func, replaceQuotes arg)
                    | TplConstr es -> TplConstr(Array.map replaceQuotes es)
                    | AsBinding(e, name) -> AsBinding(replaceQuotes e, name)
                    | Match(e, cases) -> Match(replaceQuotes e, List.map (fun (pat, guard, res) -> (replaceQuotes pat, replaceQuotes guard, replaceQuotes res)) cases)
                    | StrdLit(typ, _) -> failwithf "unparsed structured data literal of type: %s" typ
                let lexbuf = Lexing.LexBuffer<char>.FromString str
                try
                    replaceQuotes <| JsonPar.Main JsonLex.Token lexbuf
                with
                    | exn -> failwithf "json literal parsing error: %s" exn.Message
    end
