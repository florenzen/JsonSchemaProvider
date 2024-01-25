#r "nuget: FSharp.Data"

open FSharp.Quotations
open FSharp.Data

// let arg = <@@ [ 1.0; 2.0 ] @@>
// let conv = <@@ fun num -> JsonValue.Float(num) @@>

// <@@ [| ("foo", JsonValue.Array(Array.map (%%conv) (List.toArray (%%arg)))) |] @@>


<@@ List.map (fun x -> x) [1,2,3]@@>
<@@ JsonValue.Array([||]).AsArray() @@>