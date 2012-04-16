Running the interpreter
==========================================
To interpret Fb programs, create a new interpreter instance in an F# script and use either ParseProgramFromString (using the program as a string) or ParseProgramFromFile (using the filename containing the program).

Registering new data literal parser
==========================================
To use a literal parser implement the IStructuredDataLiteralParser interface and use the interpreter's RegisterLiteralParser method.