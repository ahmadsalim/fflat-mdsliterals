// Learn more about F# at http://fsharp.net
module Program

open FbIntepret

let interpret =
    printfn "%A" << run << fromString
    
interpret "let plus x y = x + y in plus 2 3 end"
ignore <| System.Console.ReadLine()