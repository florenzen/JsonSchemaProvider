(**
---
title: Manual
category: Manual
categoryindex: 1
index: 1
---
*)

(*** condition: prepare ***)
#r "../../src/JsonSchemaProvider.DesignTime/bin/Debug/netstandard2.1/JsonSchemaProvider.DesignTime.dll"
#r "../../src/JsonSchemaProvider.Runtime/bin/Debug/netstandard2.1/JsonSchemaProvider.Runtime.dll"
#r "../../src/JsonSchemaProvider.Runtime/bin/Debug/netstandard2.1/FSharp.Data.Json.Core.dll"
#r "../../src/JsonSchemaProvider.Runtime/bin/Debug/netstandard2.1/NJsonSchema.dll"

(**
# Scope

The JSON Schema type provider only supports JSON schemas that have an `object` type at the root level. That is,
the provider is able to deduce a type from the the following schema:

```json
{
  "type": "object",
  "properties": {
    "X": {
      "type": "string"
    },
    "Y": {
      "type": "string"
    }
  }
}
```

In contrast, it will, e.g., fail on this schema at compile-time:

```json
{
  "type": "array",
  "items": {
    "type": "int"
  }
}
```

The rationale is that a JSON array should be mapped to an F# array. This would entail
that the provided type is only a synonym for an array type and not a distinguished provided type.

The following table show the supported JSON schema types and the F# types they are
mapped to:

| **JSON Schema type** | **F# type** |
|----------------------|-------------|
| `string`             | `string`    |
| `boolean`            | `bool`      |
| `integer`            | `int`       |
| `number`             | `float`     |
| `array`              | array       |
| `object`             | class       |
*)

(**
# Usage

Add the `JsonSchemaProvider` NuGet package to your project:

```bash
dotnet add package JsonSchemaProvider
```

A JSON schema can either be given as a literal string or by a file path.

Open the `JsonSchemaProvider` namespace and pass the literal string to
the `JsonSchemaProvider` via the `schema` argument to provide a type
from that schema (`Xyz` in this case):
*)

open JsonSchemaProvider

[<Literal>]
let schema =
    """{
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

type Xyz = JsonSchemaProvider<schema=schema>

(**
## Creating values of the provided type

There are two ways to create values of the provided type:

You can use the static `Create` method that takes keyword arguments
for the properties of the root-level `object`:
*)

let xz1 = Xyz.Create(X = "x", Z = 1)

(**
Alternatively, you can use the static `Parse` method to parse a JSON into
a value of the `Xyz` type and, at the same time, validating it:
*)

let xz2 = Xyz.Parse("""{"X": "x", "Z": 1}""")

(**
For instance, the following invalid JSON value will be rejected
*)

try
    Xyz.Parse("""{"X": 1, "Z": 1}""") |> ignore
with :? System.ArgumentException as ex ->
    printfn "Error: %s" ex.Message
(*** include-output ***)

(**
with a message indicating the expected string for the `X` property.

_Note:_ This particular error would have been prevented using the `Create` method
since assigning an `int` to the `X` property is a static type error. When parsing, this
error is delayed to runtime, of course.

Similarly, other validation errors like ranges or regular expressions are postponed to
runtime.

Here is an example:
*)

[<Literal>]
let rangeSchema =
    """{
  "type": "object",
  "properties": {
    "from": {
      "type": "integer",
      "minimum": 0
    },
    "to": {
      "type": "integer",
      "maximum": 10
    }
  }
}
"""

type Range = JsonSchemaProvider<schema=rangeSchema>

try
    Range.Create(from = 0, ``to`` = 11) |> ignore
with :? System.ArgumentException as ex ->
    printfn "Error: %s" ex.Message

(**
In the examples so for, the properties were all optional. Required properties cannot
be omitted in the arguments to `Create` without leading to a compile time error.
*)

[<Literal>]
let rangeRequiredSchema =
    """{
  "type": "object",
  "properties": {
    "from": {
      "type": "integer",
      "minimum": 0
    },
    "to": {
      "type": "integer",
      "maximum": 10
    }
  },
  "required": ["from", "to"]
}
"""

type RangeRequired = JsonSchemaProvider<schema=rangeRequiredSchema>

(**
The call

```fsharp
let range = RangeRequired.Create(from=1)
```

leads to the error
```text
The member or object constructor 'Create' requires 1 argument(s). The required signature
is 'JsonSchemaProvider<...>.Create(from: int, ``to`` : int) : JsonSchemaProvider<...>'.
```
*)

(**
## Selecting from values of the provided type

The properties of a JSON object are exposed as F# properties of the JSON value.
An optional JSON property of type `T` is exposed as F# property of type `T' option`
where `T'` is the F# type the JSON type `T` is mapped to.

Likewise, required properties are mapped directly from `T` to `T'`.

Consider the following schema to store names with middle initials as they are
common in the US:
*)

[<Literal>]
let nameSchema =
    """{
  "type": "object",
  "properties": {
    "firstName": {"type": "string"},
    "middleInitials": {"type": "string"},
    "lastName": {"type": "string"}
  },
  "required": ["firstName", "lastName"]
}"""

type Name = JsonSchemaProvider<schema=nameSchema>

(**
For the following name
*)

let name1 =
    Name.Create(firstName = "Donald", middleInitials = "EK", lastName = "Knuth")

(**
we have a `string` for the first name
*)

name1.firstName
(*** include-fsi-output ***)

(**
and a `string option` for the middle initials:
*)

name1.middleInitials
(*** include-fsi-output ***)

(**
## Nested objects

It is common that JSON schemas specify nested objects. Consider, e.g., the following
JSON Schema to store the global position of a city:
*)

[<Literal>]
let cityPosition =
    """{
  "type": "object",
  "properties": {
    "city": {"type": "string"},
    "globalPosition": {
      "type": "object",
      "properties": {
        "lat": {"type": "number"},
        "lon": {"type": "number"}
      },
      "required": ["lat", "lon"]
    }
  },
  "required": ["city", "globalPosition"]
}
"""

type CityPosition = JsonSchemaProvider<schema=cityPosition>

(**
The JSON schema provider creates inner types for nested objects that
have the name `pObj` where `p` is the name of the property with
the nested object type:
*)

let position =
    CityPosition.globalPositionObj.Create(lat = 52.520007, lon = 13.404954)

let berlinPosition = CityPosition.Create("Berlin", position)
(*** include-fsi-output ***)

(**
Nested properties can be selected in the expected way:
*)

let berlinLat = berlinPosition.globalPosition.lat
(*** include-fsi-output ***)

(**
## Arrays

JSON arrays are mapped to F# arrays:
*)

[<Literal>]
let temperatures =
    """{
  "type": "object",
  "properties": {
    "location": {"type": "string"},
    "values": {
      "type": "array",
      "items": {"type": "number"}
      }
  },
  "required": ["location", "values"]
}"""

type Temperatures = JsonSchemaProvider<schema=temperatures>

// let temps = Temperatures.Create("Munich", [ 11.0; 12.0; 11.6; 12.1 ])
