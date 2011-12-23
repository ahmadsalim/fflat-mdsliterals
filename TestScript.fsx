#r "FSharp.PowerPack.dll"
#load "FbAst.fs" "FbPar.fs" "FbLex.fs" "FbEval.fs" "FbInterpret.fs"

open FbInterpret

let interpret =
    printfn "%A" << run << fromString

fromString "%xml{ 
                   <person>
                     <name>AS</name>
                     <nick>The Man</nick>
                   </person>
                }"