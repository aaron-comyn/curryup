///
/// CurryTheF#Up
///

// Load the script
#load "curryup.fsx"
open CurryUp

let outfile = __SOURCE_DIRECTORY__ + "/testoutput.fsx"


// Generate namespace
Curry.up outfile "System.Collections.Generic" 

// Generate individual types
Curry.up outfile "System.Text.StringBuilder"
Curry.up outfile "System.Collections.Generic.SortedDictionary`2" 

// Generate for a DLL
Curry.up outfile @"C:\local\proj\mydll.dll" 


// Piped examples
@"System.Collections"                |> Curry.up outfile
@"System.Collections.Generic.List`1" |> Curry.up outfile
@"/proj/src/bin/release/my.dll"      |> Curry.up outfile


// Full configuration
let config = 
    { Curry.DefaultConfig with 
          From = "System.Collections.Generic"
          To = "test.fsx"
          MethodOverload   = fun name -> name + "'"
          CurriedNamespace = fun namespace' -> namespace' + ".Curried" }
Curry.up' config
