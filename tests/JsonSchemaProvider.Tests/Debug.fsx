#r "../../src/JsonSchemaProvider.Runtime/bin/Debug/netstandard2.0/JsonSchemaProvider.dll"
#r "nuget: System.Private.Uri"
#r "nuget: System.Text.Encodings.Web"
#r "nuget: FSharp.Data.Json.Core"
#r "nuget: NJsonSchema"
#r "nuget: Expecto"
#load "JsonSchemaProviderTests.fs"

open JsonSchemaProvider.Tests

JsonSchemaProviderTests.flatSchema

// devenv.exe /debugexe fsc.exe -r:"..\..\src\JsonSchemaProvider.Runtime\bin\Debug\netstandard2.0\JsonSchemaProvider.dll" Debug.fsx