#r "FSharp.PowerPack.dll"
#load "FbAst.fs" "FbPar.fs" "FbLex.fs" "FbEval.fs" "FbInterpret.fs"

open FbInterpret

let interpret =
    printfn "%A" << run << fromString

interpret (" let even n = if n = 0 then true else odd(n-1)
             and odd  n = if n = 0 then false else even(n-1) 
             in even 55535 end")