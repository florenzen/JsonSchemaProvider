// For more information see https://aka.ms/fsharp-console-apps
open JsonSchemaProvider

// printfn "Hello from F#"

// type FooPair = Pair<prefix="Fob">

// let foo = FooPair("1", "2")

// printfn "%s" (foo.FobFst())

// printfn "%s" (foo.FobSnd())

// printfn "%s" (foo.FobSnd())

// let x = FooPair.Baz()

// printfn "%s" (x.FobSnd())


// type Data = JsonProvider<schema="...">
// Data.parse(input: string) -> Data
// let d = Data.parse(...)
// d.

open Newtonsoft.Json.Linq
open NJsonSchema
[<Literal>]
let schemaSource = """{
  "$id": "https://example.com/address.schema.json",
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "description": "An address similar to http://microformats.org/wiki/h-card",
  "type": "object",
  "properties": {
    "post-office-box": {
      "type": "string"
    },
    "street-address": {
      "type": "string"
    },
    "locality": {
      "type": "string"
    },
    "region": {
      "type": "string"
    },
    "postal-code": {
      "type": "string"
    },
    "country-name": {
      "type": "string"
    },
    "nested": {
        "type": "object",
        "properties": {
            "propa": { "type": "string"},
            "propb": { "type": "string"}
        }
    },
    "code": {
        "type": "integer"
    },
    "again": { "type": "boolean"},
    "list": {
      "type": "array",
      "items": {
        "type": "string"
      }
    },
    "List2": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "l2a": {
            "type": "string"
          },
          "l2b": {
            "type": "string"
          }
        },
        "required": ["l2b"]
      }
    }
  },
  "required": [ "locality", "region", "country-name" ]
}"""

let data = """{
    "post-office-box": "foo",
    "locality": "ff",
    "region": "d",
    "country-name": "ger",
    "nested": {"propa": "AA", "propb": "BB"},
    "code": 17,
    "again": true,
    "list": ["a","b"],
    "List2": [{"l2a": "XX", "l2b": "YY"}]
}"""

let data1 = """{
    "post-office-box": "foo",
    "locality": "ff",
    "region": "d",
}"""
let schema = JsonSchema.FromJsonAsync(schemaSource) |> Async.AwaitTask |> Async.RunSynchronously
let json = JToken.Parse(data)

printfn "%O" (schema.Validate(json))

let vali(data: string) =
    let json =JToken.Parse(data)
    let result = schema.Validate(json)
    Seq.isEmpty result

// printf "%b" (vali data)
// printf "%s" (JToken.op_Explicit(json["post-office-box"]))
printfn "%O" (JToken.op_Explicit(json["again"]):bool)
type Addr = JsonSchemaProvider<schema=schemaSource>

let d = Addr.Parse(data)

printfn "%s" (d.region)
printfn "%O" (d.nested |> Option.map (fun x -> x.propa))
printfn "%d" (d.code |> Option.get)
printfn "%O" (d.again)
printfn "%O" (d.List2 |> List.head |> fun x -> x.l2b)


[<Literal>]
let schemaSource2 = """{
  "$id": "https://example.com/address.schema.json",
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "X": {
      "type": "string"
    },
    "Y": {
      "type": "string"
    }
  }
}"""

type Json2 = JsonSchemaProvider<schema=schemaSource2>

let c = Json2.Create(Y="xx", X="yy")

printf "%O" c