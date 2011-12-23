// Learn more about F# at http://fsharp.net
module Program

open FbIntepret

let interpret =
    printfn "%A" << run << fromString
    
interpret "let even x = if x = 0 then true else odd  (x-1)
           and odd  x = if x = 0 then false else even (x-1)
           in even 10 end"
ignore <| System.Console.ReadLine()