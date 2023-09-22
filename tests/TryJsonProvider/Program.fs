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
    "extended-address": {
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
    "again": { "type": "boolean"}
  },
  "required": [ "locality", "region", "country-name" ],
  "dependentRequired": {
    "post-office-box": [ "street-address" ],
    "extended-address": [ "street-address" ]
  }
}"""

let data = """{
    "post-office-box": "foo",
    "locality": "ff",
    "region": "d",
    "country-name": "ger",
    "nested": {"propa": "AA", "propb": "BB"},
    "code": 17,
    "again": true
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
printfn "%s" (d.nested.propa)
printfn "%d" (d.code)
printfn "%O" (d.again)