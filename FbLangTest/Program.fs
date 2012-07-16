open FbInterpret
open FbAst
open uXmlLiteralParser
open JsonLiteralParser

let interpreter = new Interpreter()
let interpret = interpreter.Interpret

let rawStringLiteralParser = mkStucturedDataLiteralParser "q" Seq.empty (fun _ str -> Cst(Str str))
interpreter.RegisterLiteralParser(rawStringLiteralParser)

let xmlLiteralParser = new UXmlLiteralParser()
interpreter.RegisterLiteralParser(xmlLiteralParser)

let jsonLiteralParser = new JsonLiteralParser()
interpreter.RegisterLiteralParser(jsonLiteralParser)

interpret @"let even? num = if num = 0 then true  else odd? (num - 1)
            and odd?  num = if num = 0 then false else even?(num - 1)
            in 
              (""Hello"", odd? 11)
            end"

interpret @"let a = [XContent(%q{Fantasy})]
            in
             let thedata = %xml{<book id=""bk105"">
                                   <author>Corets, Eva</author>
                                   <title>The Sundered Grail</title>
                                   <genre>[a]</genre>
                                   <price>5.95</price>
                                   <publish_date>2001-09-10</publish_date>
                                   <description>The two daughters of Maeve, half-sisters,
                                   battle one another for control of England. Sequel to
                                   Oberon's Legacy.</description>
                                </book>}
             in
               match thedata with
               | %xml{<book [id]='[idVal]' [attrs]>
                   <author>[authorVal]</>
                   <title>[titleVal]</>
                   [restVal]
                </>} ->
                     let jsonId = JsString(idVal)
                     and jsonAuthor = JsString(match authorVal with | [XContent(author)] -> author)
                     and jsonTitle = JsString(match titleVal with | [XContent(title)] -> title)
                     in
                       Right %json({ ""id"": <jsonId>,
                                     ""author"": <jsonAuthor>,
                                     ""title"": <jsonTitle> })
                     end
               | _                             -> Left ""Failed""
             end
            end"

interpret @"let f a b c = ((a, b), fun q u -> c q) in f end"

interpret @"type expr = 
             | Cst of value : int requires value > 0
             | Plus of er : expr * el : expr

             Plus(Cst 1, Cst 10)"

interpret @"let emptylistlist = [[]] 
           in 
              ([2, 1]::emptylistlist, [true]::emptylistlist, [emptylistlist])
           end"