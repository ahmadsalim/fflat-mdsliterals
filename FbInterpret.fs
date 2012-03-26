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


type IStructuredDataLiteralParser =
  interface
    abstract member HandledDataType : string with get
    abstract member ResultDataTypes : datadecl seq with get
    abstract member ParseLiteral : parser : (string -> expr) -> literal : string -> expr
  end


let makeStucturedDataLiteralParser (handledDataType : string) (resultDataTypes : datadecl seq) (parseLiteral : (string -> expr) -> string -> expr) =
   {  new IStructuredDataLiteralParser with
        member this.HandledDataType = handledDataType
        member this.ResultDataTypes = resultDataTypes
        member this.ParseLiteral(parser)(expr) = parseLiteral(parser)(expr)
   }

type Interpreter() =
  class
    let strdlitParsers =
      new System.Collections.Generic.Dictionary<string, IStructuredDataLiteralParser>()
    member public this.RegisterLiteralParser (parser : IStructuredDataLiteralParser) : unit =
      strdlitParsers.Add(parser.HandledDataType, parser)

    member public this.ParseExpressionFromString (str : string) : expr =
          let lexbuf = Lexing.LexBuffer<char>.FromString(str)
          try
             FbPar.Expr FbLex.Token lexbuf
          with
          | exn -> let pos = lexbuf.EndPos
                   failwithf "%s near line %d, column %d\n"
                     (exn.Message) (pos.Line+1) pos.Column
    member public this.ParseProgramFromString (str : string) : typeenv * expr =
          let lexbuf = Lexing.LexBuffer<char>.FromString(str)
          try
              FbPar.Main FbLex.Token lexbuf
          with
          | exn -> let pos = lexbuf.EndPos
                   failwithf "%s near line %d, column %d\n"
                       (exn.Message) (pos.Line+1) pos.Column
    member public this.ParseProgramFromFile (filename : string) : typeenv * expr =
         use reader = new StreamReader(filename)
         let lexbuf = Lexing.LexBuffer<char>.FromTextReader reader
         try
            FbPar.Main FbLex.Token lexbuf
         with
            | exn -> let pos = lexbuf.EndPos
                     failwithf "%s in file %s near line %d, column %d\n"
                      (exn.Message) filename (pos.Line+1) pos.Column
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
       let parsed = this.ParseProgramFromString str
       let checkd = checkExpr (snd parsed)
       let ran = this.Run (fst parsed) (snd parsed)
       let prettied = this.PrettyPrintString ran
       printfn "%s" prettied
  end