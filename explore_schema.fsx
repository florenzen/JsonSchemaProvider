#r "nuget: NJsonSchema"
#r "nuget: FSharp.Data"

let nestedArray =
    """{
      "type": "object",
      "properties": {
        "values": {
          "type": "array",
          "items": {
            "type": "array",
            "items": {"type": "string"}
          }
        }
      },
      "required": ["values"]
    }"""

open NJsonSchema


s.Properties["values"].Item.Item.Type
s.Properties["values"].Type

let rec traverse (prop: JsonSchema) =

    printfn "%s" (prop.ToString())

    match prop.Type with
    | JsonObjectType.Array -> "[" + traverse prop.Item + "]"
    | _ -> prop.Type.ToString()

traverse s.Properties["values"]

open FSharp.Quotations
open FSharp.Data
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Reflection
open FSharp.Data
// let fSharpCore = typeof<List<_>>.Assembly

// let listModuleType =
//     fSharpCore.GetTypes() |> Array.find (fun ty -> ty.Name = "ListModule")

// let miToArray =
//     listModuleType.GetMethods()
//     |> Array.find (fun methodInfo -> methodInfo.Name = "ToArray")
//     |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(toType)

let rec tyFromJsonSchema (s: JsonSchema) =
    match s.Type with
    | JsonObjectType.Integer -> typeof<int>
    | JsonObjectType.String -> typeof<string>
    | JsonObjectType.Number -> typeof<float>
    | JsonObjectType.Boolean -> typeof<bool>
    
    | JsonObjectType.Array ->
      let innerTy = tyFromJsonSchema s.Item
      let genList = typedefof<list<_>>
      genList.MakeGenericType(innerTy)

let rec selectFromJsonValue (s: JsonSchema) =
    let asInt = <@@ fun (j: JsonValue) -> j.AsInteger() @@>

    fun (args: Expr list) ->
        (match s.Type with
         | JsonObjectType.Array -> failwith "nyi" // j.AsArray() |> Array.toList |> List.map (fun x -> x.AsInteger())
           // j.AsArray() |> Array.toList |> List.map (fun x -> x.AsArray() |> Array.toList |> List.map (fun x -> x .AsInteger()))
         | JsonObjectType.Integer -> Expr.Application asInt (args[0]))
         
