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

[<Literal>]
let schemaSource2 =
    """{
  "$id": "https://example.com/address.schema.json",
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "X": {
      "type": "string"
    },
    "Y": {
      "type": "string"
    },
    "Z": {
      "type": "integer"
    },
    "nested": {
      "type": "object",
      "properties": {
        "nestedA": {"type": "string"},
        "nestedB": {"type": "string"}
      }
    }
  }
}"""

[<Literal>]
let schemaSource3 =
    """{
  "$id": "https://example.com/address.schema.json",
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "X": {
      "type": "string"
    },
    "Y": {
      "type": "string"
    },
    "Z": {
      "type": "integer"
    }
  }
}"""

type Json2 = JsonSchemaProvider<schema=schemaSource2>

let c =
    Json2.Create(Y = "xx", X = "yy", nested = Json2.nestedObj.Create(nestedA = "A", nestedB = "B"))

printf "%O" c
printf "%O" (Json2.nestedObj.Create(nestedA = "A"))

// printfn "%O" Json2.nestedObj
