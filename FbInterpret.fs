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


let run (types, e) = eval e [] types