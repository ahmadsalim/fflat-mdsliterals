(*
    Intepreter for the Fb programming language
    Original Author: Peter Sestoft <sestoft@itu.dk> (for the the microML programming language)
*)
module FbInterpret

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing
open FbAst
open FbEval
open FbCheck
open FbType

type IStructuredDataLiteralParser =
  interface
    abstract member HandledDataType : string with get
    abstract member ResultDataTypes : datadecl seq with get
    abstract member ParseLiteral : parser : (string -> expr) -> literal : string -> expr
  end


let mkStucturedDataLiteralParser (handledDataType : string) (resultDataTypes : datadecl seq) (parseLiteral : (string -> expr) -> string -> expr) =
   {  new IStructuredDataLiteralParser with
        member this.HandledDataType = handledDataType
        member this.ResultDataTypes = resultDataTypes
        member this.ParseLiteral(parser)(expr) = parseLiteral(parser)(expr)
   }

type Interpreter() =
  class
    let registeredTypes = ref Map.empty
    let strdLitParsers =
      new System.Collections.Generic.Dictionary<string, IStructuredDataLiteralParser>()

    member public this.RegisterLiteralParser (parser : IStructuredDataLiteralParser) : unit =
      registeredTypes := Seq.fold (fun typeenv decl -> addDataDeclToEnv decl typeenv) !registeredTypes parser.ResultDataTypes
      strdLitParsers.Add(parser.HandledDataType, parser)

    member public this.ParseExpressionFromString (str : string) : expr =
          let lexbuf = Lexing.LexBuffer<char>.FromString(str)
          try
             FbPar.Expr FbLex.Token lexbuf
          with
          | exn -> let pos = lexbuf.EndPos
                   failwithf "%s near line %d, column %d\n"
                     (exn.Message) (pos.Line+1) pos.Column
    member public this.ParseProgramFromString (str : string) : datadecl list * expr =
          let lexbuf = Lexing.LexBuffer<char>.FromString(str)
          try
              FbPar.Main FbLex.Token lexbuf
          with
          | exn -> let pos = lexbuf.EndPos
                   failwithf "%s near line %d, column %d\n"
                       (exn.Message) (pos.Line+1) pos.Column

    member public this.ParseProgramFromFile (filename : string) : datadecl list * expr =
         use reader = new StreamReader(filename)
         let lexbuf = Lexing.LexBuffer<char>.FromTextReader reader
         try
            FbPar.Main FbLex.Token lexbuf
         with
            | exn -> let pos = lexbuf.EndPos
                     failwithf "%s in file %s near line %d, column %d\n"
                      (exn.Message) filename (pos.Line+1) pos.Column

    member public this.ResolveStructuredDataLiterals (expression : expr) : expr =
         let rec resolveStrdLit expr =
            match expr with
            | StrdLit(datatype, literal) when strdLitParsers.ContainsKey(datatype) ->
                checkStrdLit (strdLitParsers.[datatype].ParseLiteral
                                  (this.ParseExpressionFromString) (literal)) datatype
            | StrdLit(datatype, literal) ->
                failwithf "Unregistered parser for structured data literal of type %s" datatype
            | Lets(bs, eb) ->
                  Lets(List.map (fun (name, er) -> (name, resolveStrdLit er)) bs, resolveStrdLit eb)
            | Prim(opr, e1, e2) -> Prim(opr, resolveStrdLit e1, resolveStrdLit e2)
            | If(ec, et, ef)    -> If(resolveStrdLit ec, resolveStrdLit et, resolveStrdLit ef)
            | Letfuns(efuns, eb) ->
                Letfuns(List.map (fun (fname, fparam, er) -> (fname, fparam, resolveStrdLit er)) efuns,
                          resolveStrdLit eb)
            | Fun(param, eb) -> Fun(param, resolveStrdLit eb)
            | Call(efun, eparam) -> Call(resolveStrdLit efun, resolveStrdLit eparam)
            | TplConstr(es) -> TplConstr(Array.map resolveStrdLit es)
            | Match(edata, cases) ->
                Match(resolveStrdLit edata,
                       List.map (fun (p, g, r) -> (resolveStrdLit p, resolveStrdLit g, resolveStrdLit r)) cases)
            | AsBinding(e, n) -> AsBinding(resolveStrdLit e, n)
            | AdtConstr _ -> expr
            | Cst _ -> expr
            | Var _ -> expr
            | WildCard -> expr
            | Quote _ -> expr
         resolveStrdLit expression

    member public this.Run (types : typeenv) (e : expr) : value =
         eval e [] types

    member public this.PrettyPrintString (vl : value) : string =
         let rec prettyPrint vl =
          match vl with
          | Val(Bool b) -> sprintf "%A" b
          | Val(Int i)  -> sprintf "%d" i
          | Val(Str s)  -> sprintf "%A" s
          | Tpl(vals)   -> sprintf "(%s)" (System.String.Join(",", Array.map (prettyPrint) vals))
          | Adt("@Nil", _) -> "[]"
          | Adt("@Cons", [| head; tail |] ) ->
              let rec prettyPrintCons head tail first =
                   if first
                    then sprintf "%s" ((prettyPrint head) + (match tail with
                                                             | Adt("@Cons", [| head; tail |]) -> prettyPrintCons head tail false
                                                             | Adt("@Nil", _) -> ""
                                                             | _ -> failwith "Error in list ADT"))
                    else sprintf ", %s" ((prettyPrint head) + (match tail with
                                                               | Adt("@Cons", [| head; tail |]) -> prettyPrintCons head tail false
                                                               | Adt("@Nil", _) -> ""
                                                               | _ -> failwith "Error in list ADT"))
              sprintf "[%s]" (prettyPrintCons head tail true)
          | Adt(name, vals) -> if vals.Length = 0
                                then sprintf "%s" name
                                else sprintf "%s(%s)" name (System.String.Join(",", Array.map (prettyPrint) vals))
          | Closure _    -> sprintf "fun<%x>" (vl.GetHashCode())
          | AdtClosure _ -> sprintf "adt<%x>" (vl.GetHashCode())
         prettyPrint vl

    member public this.Interpret (str : string) : unit =
       let (decls, parsed) = this.ParseProgramFromString str
       let replaced = this.ResolveStructuredDataLiterals parsed
       let checkd = checkExpr replaced
       let env = List.fold (fun types decl -> addDataDeclToEnv decl types) !registeredTypes decls
       let typed = inferType checkd env 
       let ran = this.Run env checkd
       let prettied = this.PrettyPrintString ran
       printfn "%s : %s" prettied typed
  end