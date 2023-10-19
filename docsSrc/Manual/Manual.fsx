(**
---
title: Manual
category: Manual
categoryindex: 1
index: 1
---
*)

(**
# Usage

Add the `JsonSchemaProvider` NuGet package to your project:

```bash
dotnet add package JsonSchemaProvider
```

To 
*)

(*** condition: prepare ***)
#r "../../src/JsonSchemaProvider.Runtime/bin/Debug/netstandard2.1/JsonSchemaProvider.Runtime.dll"
#r "../../src/JsonSchemaProvider.Runtime/bin/Debug/netstandard2.1/FSharp.Data.Json.Core.dll"
#r "../../src/JsonSchemaProvider.Runtime/bin/Debug/netstandard2.1/NJsonSchema.dll"

(**

*)

open JsonSchemaProvider

[<Literal>]
let schema =
    """
{
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
  }
}"""

type Abc = JsonSchemaProvider<schema=schema>

let abc = Abc.Create(X = "x", Z = 1)
abc
(*** include-it ***)

abc.Z
(*** include-it ***)

abc.Y
None
(*** include-it ***)
