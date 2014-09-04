### Main Page
The page of this project is located at [http://ahmadsalim.github.com/fflat-mdsliterals/](http://ahmadsalim.github.com/fflat-mdsliterals/)

### Overview
Modular Structured Data Literals extend [Fb](http://ahmadsalim.github.com/fflat/) with the ability to support literals of various kinds of structured data, such as XML, JSON and YAML.
This extension allows integration of new literal parsers, and supports both construction of and pattern matching on literal expressions.

#### Example

```ocaml
let a = [XContent(%q{Fantasy})]
in
  let thedata =
    %xml{
      <book id="bk105">
        <author>Corets, Eva</author>
        <title>The Sundered Grail</title>
        <genre>[a]</genre>
        <price>5.95</price>
        <publish_date>2001-09-10</publish_date>
        <description>
          The two daughters of Maeve, half-sisters,
          battle one another for control of England. 
          Sequel to Oberon's Legacy
        </description>
      </book>
    }
  in
    match thedata with
    | %xml{
        <book [id]='[idVal]' [attrs]>
          <author>[authorVal]</>
          <title>[titleVal]</>
          [restVal]
        </>
      }   ->
             let jsonId = 
               JsString(idVal)
             and jsonAuthor = 
               JsString(match authorVal with 
                        | [XContent(author)] -> author)
             and jsonTitle = 
               JsString(match titleVal with 
                        | [XContent(title)] -> title)
             in
               Right %json(
               { 
                 "id": <jsonId>,
                 "author": <jsonAuthor>,
                 "title": <jsonTitle> 
               })
             end
      | _ -> Left "Failed"
    end
end
```

### Running the interpreter
To interpret Fb programs with Modular Structured Data Literals, create a new interpreter instance in an F# script and use either ParseProgramFromString (using the program as a string) or ParseProgramFromFile (using the filename containing the program).

### Registering new data literal parser
To use a literal parser implement the IStructuredDataLiteralParser interface and use the interpreter's RegisterLiteralParser method.

### Authors and Contributors
The main author behind this project is Ahmad Salim AlSibahi (@ahmadsalim).
The language and compiler is based on Peter Sestoft(@sestoft)'s µML langauge.
