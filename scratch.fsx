
// CurryTheF#Up

#load "curryup.fsx"
open CurryUp

let outfile = __SOURCE_DIRECTORY__ + "/testoutput.fsx"
Curry.up outfile "System.Text.StringBuilder"
Curry.up outfile "System.Collections.Generic" 

#load "testoutput.fsx"



// Passing enums

Curry.up outfile "System.Drawing.Graphics"
let dr = ((typeof<System.Drawing.Graphics>).GetMethods() |> Array.filter (fun m -> m.Name.StartsWith("Enumerate"))).[0] // |> Seq.map (fun m -> m.Name) |> Seq.sort |> Seq.toList 


 



// TryGetBuffer testing:  method is "invisible" to call, but listed as a public method...

let tt = (typeof<System.IO.MemoryStream>).GetMethods() // |> Seq.map (fun m -> counter <- counter + 1; counter, m.Name) |> Seq.toList |> List.sort
let tgb   = tt.[6]
let write = tt.[5] //.Attributes
tgb.GetBaseDefinition()
open System.Reflection
let rrr =new System.IO.MemoryStream()
rrr.GetType().GetMethods(BindingFlags.Instance ||| BindingFlags.Public) |> Seq.map (fun m -> m.Name) |> Seq.toList |> List.sort



