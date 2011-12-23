// Learn more about F# at http://fsharp.net
module Program

open FbIntepret

let interpret =
    printfn "%A" << run << fromString
    
interpret @"""Hello "" ^ ""World"""
ignore <| System.Console.ReadLine()