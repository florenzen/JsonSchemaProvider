#r "nuget: NJsonSchema"

let nestedArray =
    """{
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "values": {
          "type": "booxxl"
        }
      },
      "required": ["values"]
    }"""

open NJsonSchema

let parseSchema (schemaSource: string) =
    JsonSchema.FromJsonAsync(schemaSource)
    |> Async.AwaitTask
    |> Async.RunSynchronously

let x = (parseSchema nestedArray)

printfn "%O" x

