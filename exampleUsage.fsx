// CurryTheF#Up

#load "curryup.fsx"
open CurryUp

let outfile = __SOURCE_DIRECTORY__ + "/testoutput.fsx"

Curry.up outfile "System.Type"
Curry.up outfile "System.Text.StringBuilder"
Curry.up outfile "System.IO"
Curry.up outfile "System.Text"
Curry.up outfile "System.Security"
Curry.up outfile "System.Collections.Generic.SortedDictionary`2" 

@"System.Collections.Generic"        |> Curry.up outfile
@"System.Collections.Generic.List`1" |> Curry.up outfile
@"/proj/src/bin/release/my.dll"      |> Curry.up outfile

let config = 
    { Curry.DefaultConfig with 
          From = "System.Collections.Generic"
          To = "test.fsx"
          MethodOverload   = fun name -> name + "'"
          CurriedNamespace = fun namespace' -> namespace' + ".Curried" }
Curry.up' config

#load "testoutput.fsx"
//open System.Text.StringBuilder.Curried.StringBuilder
//open System.IO.File.Curried.File 
//module sb = System.Text.StringBuilder.Curried.StringBuilder
//module f = System.IO.File.Curried.File 

