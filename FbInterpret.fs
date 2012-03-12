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

(* Plain parsing from a string, with poor error reporting *)

let fromString (str : string) : typeenv * expr =
    let lexbuf = Lexing.LexBuffer<char>.FromString(str)
    in try
         FbPar.Main FbLex.Token lexbuf
       with
         | exn -> let pos = lexbuf.EndPos
                  in failwithf "%s near line %d, column %d\n"
                     (exn.Message) (pos.Line+1) pos.Column

(* Parsing from a file *)

let fromFile (filename : string) =
    use reader = new StreamReader(filename)
    let lexbuf = Lexing.LexBuffer<char>.FromTextReader reader
    in try
         FbPar.Main FbLex.Token lexbuf
       with
         | exn -> let pos = lexbuf.EndPos
                  in failwithf "%s in file %s near line %d, column %d\n"
                     (exn.Message) filename (pos.Line+1) pos.Column


(* Running a parsed string *)
let run (types, e) = eval e [] types

(* Getting pretty printing output string *)
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
    | Adt(name, vals) -> sprintf "%s(%s)" name (System.String.Join(",", Array.map (prettyPrint) vals))
    | Closure _    -> sprintf "fun<%x>" (vl.GetHashCode())
    | AdtClosure _ -> sprintf "adt<%x>" (vl.GetHashCode())

(* Interpreting a string (parsing, running, and pretty printing its output) *)
let interpret =
    printfn "%s" << prettyPrint << run << fromString