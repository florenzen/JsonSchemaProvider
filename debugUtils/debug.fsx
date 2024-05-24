#load "/workspaces/JsonSchemaProvider/debugUtils/references.fsx"

// This is an example script to debug the type provider code when
// executed by fsi. The "fsi debugUtils/debug.fsx" launch configuration can
// be used to run the script in the debugger.
// Breakpoints can be set in the code of the type provider.

open JsonSchemaProvider

[<Literal>]
let flatObject =
    """
    {
      "type": "object",
      "properties": {
        "X": { "type": "string" },
        "Y": { "type": "string" },
        "Z": { "type": "integer" }
      },
      "required": ["Y"]
    }"""

type FlatObject = JsonSchemaProvider<schema=flatObject>

let fo0 = FlatObject.Create(Y = "y", Z = 1)

let fo1 = FlatObject.Parse("""{"X": "x", "Y": "y", "Z": 1}""")

printfn "%A %A" fo1.X fo1.Y

let fo2 = FlatObject.Parse("""{"Y": "y", "X": "x"}""")

printfn "%A %A %A" fo2.X fo2.Y fo2.Z

[<Literal>]
let nestedObjects =
    """
    {
      "type": "object",
      "properties": {
        "header": {
          "type": "object",
          "properties": {
            "id": {"type": "integer"},
            "sender": {"type": "string"},
            "resend": {"type": "boolean"},
            "time": {
              "type": "object",
              "properties": {
                "hour": {"type": "integer"},
                "minute": {"type": "integer"},
                "second": {"type": "integer"}
              },
              "required": ["hour", "minute", "second"]
            }
          },
          "required": ["id", "sender"]
        },
        "body": {
          "type": "object",
          "properties": {
            "length": {"type": "integer"},
            "payload": {"type": "string"}
          },
          "required": ["length", "payload"]
        },
        "addendum": {"type": "string"}
      },
      "required": ["body"]
    }"""

type NestedObjects = JsonSchemaProvider<schema=nestedObjects>

let no1 =
    NestedObjects.Parse(
        """
        {
          "header": {"id": 12, "sender": "remote"},
          "body": {
            "length": 1,
            "payload": "U"
          }
        }"""
    )

printfn
    "%A %A %A %A %A %A"
    no1.body.length
    no1.body
    no1.body.payload
    no1.header
    no1.header.Value
    no1.header.Value.resend

printfn
    "%A"
    (NestedObjects.Create(
        body = NestedObjects.bodyObj.Create(10, "tententen1"),
        ?header =
            (NestedObjects.headerObj.Create(
                11,
                "sender",
                resend = false,
                time = NestedObjects.headerObj.timeObj.Create(10, 10, 10)
             )
             |> Some)
    ))

[<Literal>]
let nestedArrayWithObjectItems =
    """
    {
      "type": "object",
      "properties": {
        "values": {
          "type": "array",
          "items": {
            "type": "array",
            "items": {
              "type": "object",
              "properties": {
                "propA": {"type": "integer"},
                "propB": {"type": "string"}
              }
            }
          }
        }
      }
    }"""

type NestedArrayWithObjectItems = JsonSchemaProvider<schema=nestedArrayWithObjectItems>

let nawoi0 =
    NestedArrayWithObjectItems.Create([ [ NestedArrayWithObjectItems.valuesObj.Create(propA = 5) ] ])

let nawoi1 =
    NestedArrayWithObjectItems.Parse(
        """
        {
          "values": [
            [
              {"propA": 1, "propB": "x"},
              {"propA": 2, "propB": "y"}
            ],
            [
              {"propA": 10, "propB": "xx"},
              {"propA": 20}
            ]
          ]
        }"""
    )

printfn "%A" (nawoi1.values.Value[1][1]).propB

printfn
    "%A"
    (NestedArrayWithObjectItems.Create(
        [ [ NestedArrayWithObjectItems.valuesObj.Create()
            NestedArrayWithObjectItems.valuesObj.Create(1, "x") ] ]
    ))

(NestedArrayWithObjectItems.Create(
    [ [ NestedArrayWithObjectItems.valuesObj.Create()
        NestedArrayWithObjectItems.valuesObj.Create(1, "x") ] ]
))
