﻿// Copyright (c) 2023 Florian Lorenzen

// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the “Software”), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:

// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

namespace JsonSchemaProvider.DesignTime

open System.IO
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open NJsonSchema
open FSharp.Data
open JsonSchemaProvider

[<TypeProvider>]
type JsonSchemaProviderImpl(config: TypeProviderConfig) as this =
    inherit
        TypeProviderForNamespaces(
            config,
            assemblyReplacementMap = [ ("JsonSchemaProvider.DesignTime", "JsonSchemaProvider.Runtime") ],
            addDefaultProbingLocation = true
        )

    let namespaceName = "JsonSchemaProvider"
    let thisAssembly = Assembly.GetExecutingAssembly()

    let staticParams =
        [ ProvidedStaticParameter("schema", typeof<string>, "")
          ProvidedStaticParameter("schemaFile", typeof<string>, "") ]

    let baseTy = typeof<NullableJsonValue>

    let jsonSchemaTy =
        ProvidedTypeDefinition(thisAssembly, namespaceName, "JsonSchemaProvider", baseType = Some baseTy)

    let rec determineReturnType
        (name: string)
        (item: JsonSchema)
        (schema: JsonSchema)
        (ty: ProvidedTypeDefinition)
        (isRequired: bool)
        (propType: JsonObjectType)
        =
        match propType with
        | JsonObjectType.String -> if isRequired then typeof<string> else typeof<string option>
        | JsonObjectType.Boolean -> if isRequired then typeof<bool> else typeof<bool option>
        | JsonObjectType.Integer -> if isRequired then typeof<int> else typeof<int option>
        | JsonObjectType.Number -> if isRequired then typeof<float> else typeof<float option>
        | JsonObjectType.Array ->
            let elementTy = determineReturnType name item.Item item ty true item.Type

            let list = typedefof<list<_>>
            list.MakeGenericType(elementTy)
        | JsonObjectType.Object ->
            let innerTy =
                ProvidedTypeDefinition(thisAssembly, namespaceName, name + "Obj", baseType = Some baseTy)

            generatePropertiesAndCreateForObject innerTy schema |> ignore

            ty.AddMember(innerTy)

            if isRequired then
                innerTy
            else
                let opt = typedefof<option<_>>
                opt.MakeGenericType(innerTy)
        | _ -> failwithf "Unsupported type %O" propType

    and generatePropertiesAndCreateForObject (ty: ProvidedTypeDefinition) (schema: JsonSchema) =
        let properties = schema.Properties
        let requiredProperties = schema.RequiredProperties

        let parametersForCreate =
            [ for prop in properties do
                  let name = prop.Key
                  let propType = prop.Value.Type
                  let isRequired = requiredProperties.Contains(name)

                  let returnType =
                      determineReturnType name prop.Value.Item prop.Value ty isRequired propType

                  let property =
                      ProvidedProperty(
                          propertyName = name,
                          propertyType = returnType,
                          getterCode =
                              if isRequired then
                                  match propType with
                                  | JsonObjectType.String ->
                                      fun args ->
                                          <@@
                                              let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]
                                              jsonVal.AsString()
                                          @@>
                                  | JsonObjectType.Boolean ->
                                      fun args ->
                                          <@@
                                              let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]
                                              jsonVal.AsBoolean()
                                          @@>
                                  | JsonObjectType.Integer ->
                                      fun args ->
                                          <@@
                                              let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]
                                              jsonVal.AsInteger()
                                          @@>
                                  | JsonObjectType.Number ->
                                      fun args ->
                                          <@@
                                              let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]
                                              jsonVal.AsFloat()
                                          @@>
                                  | JsonObjectType.Array ->
                                      match prop.Value.Item.Type with
                                      | JsonObjectType.String ->
                                          fun args ->
                                              <@@
                                                  let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]

                                                  jsonVal.AsArray()
                                                  |> List.ofArray
                                                  |> List.map (fun jsonVal -> jsonVal.AsString())
                                              @@>
                                      | JsonObjectType.Boolean ->
                                          fun args ->
                                              <@@
                                                  let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]

                                                  jsonVal.AsArray()
                                                  |> List.ofArray
                                                  |> List.map (fun jsonVal -> jsonVal.AsBoolean())
                                              @@>
                                      | JsonObjectType.Integer ->
                                          fun args ->
                                              <@@
                                                  let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]

                                                  jsonVal.AsArray()
                                                  |> List.ofArray
                                                  |> List.map (fun jsonVal -> jsonVal.AsInteger())
                                              @@>
                                      | JsonObjectType.Number ->
                                          fun args ->
                                              <@@
                                                  let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]

                                                  jsonVal.AsArray()
                                                  |> List.ofArray
                                                  |> List.map (fun jsonVal -> jsonVal.AsFloat())
                                              @@>
                                      | JsonObjectType.Object ->
                                          fun args ->
                                              <@@
                                                  let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]
                                                  jsonVal.AsArray() |> List.ofArray
                                              @@>
                                      | _ -> failwithf "Unsupported type %O" propType
                                  | JsonObjectType.Object ->
                                      fun args -> <@@ (%%args[0]: NullableJsonValue).JsonVal[name] @@>
                                  | _ -> failwithf "Unsupported type %O" propType
                              else
                                  match propType with
                                  | JsonObjectType.String ->
                                      fun args ->
                                          <@@
                                              let maybeJsonVal =
                                                  (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                                              maybeJsonVal
                                              |> Option.map (fun (jsonVal: JsonValue) -> jsonVal.AsString())
                                          @@>
                                  | JsonObjectType.Boolean ->
                                      fun args ->
                                          <@@
                                              let maybeJsonVal =
                                                  (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                                              maybeJsonVal
                                              |> Option.map (fun (jsonVal: JsonValue) -> jsonVal.AsBoolean())
                                          @@>
                                  | JsonObjectType.Integer ->
                                      fun args ->
                                          <@@
                                              let maybeJsonVal =
                                                  (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                                              maybeJsonVal
                                              |> Option.map (fun (jsonVal: JsonValue) -> jsonVal.AsInteger())
                                          @@>
                                  | JsonObjectType.Number ->
                                      fun args ->
                                          <@@
                                              let maybeJsonVal =
                                                  (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                                              maybeJsonVal |> Option.map (fun (jsonVal: JsonValue) -> jsonVal.AsFloat())
                                          @@>
                                  | JsonObjectType.Array ->
                                      match prop.Value.Item.Type with
                                      | JsonObjectType.String ->
                                          fun args ->
                                              <@@
                                                  let maybeJsonVal =
                                                      (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                                                  match maybeJsonVal with
                                                  | None -> List.empty
                                                  | Some(jsonVal) ->
                                                      jsonVal.AsArray()
                                                      |> List.ofArray
                                                      |> List.map (fun jsonVal -> jsonVal.AsString())
                                              @@>
                                      | JsonObjectType.Boolean ->
                                          fun args ->
                                              <@@
                                                  let maybeJsonVal =
                                                      (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                                                  match maybeJsonVal with
                                                  | None -> List.empty
                                                  | Some(jsonVal) ->
                                                      jsonVal.AsArray()
                                                      |> List.ofArray
                                                      |> List.map (fun jsonVal -> jsonVal.AsBoolean())
                                              @@>
                                      | JsonObjectType.Integer ->
                                          fun args ->
                                              <@@
                                                  let maybeJsonVal =
                                                      (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                                                  match maybeJsonVal with
                                                  | None -> List.empty
                                                  | Some(jsonVal) ->
                                                      jsonVal.AsArray()
                                                      |> List.ofArray
                                                      |> List.map (fun jsonVal -> jsonVal.AsInteger())
                                              @@>
                                      | JsonObjectType.Number ->
                                          fun args ->
                                              <@@
                                                  let maybeJsonVal =
                                                      (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                                                  match maybeJsonVal with
                                                  | None -> List.empty
                                                  | Some(jsonVal) ->
                                                      jsonVal.AsArray()
                                                      |> List.ofArray
                                                      |> List.map (fun jsonVal -> jsonVal.AsFloat())
                                              @@>
                                      | JsonObjectType.Object ->
                                          fun args ->
                                              <@@
                                                  let maybeJsonVal =
                                                      (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                                                  match maybeJsonVal with
                                                  | None -> List.empty
                                                  | Some(jsonVal) -> jsonVal.AsArray() |> List.ofArray

                                              @@>
                                      | _ -> failwithf "Unsupported type %O" propType
                                  | JsonObjectType.Object ->
                                      fun args -> <@@ (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name) @@>
                                  | _ -> failwithf "Unsupported type %O" propType
                      )

                  ty.AddMember(property)

                  yield
                      if isRequired then
                          (ProvidedParameter(name, returnType, false), propType, isRequired)
                      else
                          let returnTypeWithoutOption = returnType.GenericTypeArguments[0]

                          let (nullableReturnType, defaultValue) =
                              if returnTypeWithoutOption.IsValueType then
                                  ((typedefof<System.Nullable<_>>).MakeGenericType([| returnTypeWithoutOption |]),
                                   System.Nullable() :> obj)
                              else
                                  (returnTypeWithoutOption, null :> obj)

                          (ProvidedParameter(name, nullableReturnType, false, defaultValue), propType, isRequired) ]

        let create =
            let processArgs (args: Quotations.Expr list) =
                [ for (arg, (parameter, propType, isRequired)) in List.zip args parametersForCreate do
                      let name = parameter.Name

                      yield
                          (match (propType, isRequired) with
                           | (JsonObjectType.String, true) -> <@@ [| (name, JsonValue.String(%%arg: string)) |] @@>
                           | (JsonObjectType.String, false) ->
                               <@@
                                   match %%arg: string with
                                   | null -> [||]
                                   | value -> [| (name, JsonValue.String(value)) |]
                               @@>
                           | (JsonObjectType.Integer, true) ->
                               <@@ [| (name, JsonValue.Float((%%arg: int) |> float)) |] @@>
                           | (JsonObjectType.Integer, false) ->
                               <@@
                                   if (%%arg: System.Nullable<int>).HasValue then
                                       [| (name, JsonValue.Float((%%arg: System.Nullable<int>).Value)) |]
                                   else
                                       [||]
                               @@>
                           | (JsonObjectType.Number, true) ->
                               <@@ [| (name, JsonValue.Float((%%arg: float) |> float)) |] @@>
                           | (JsonObjectType.Number, false) ->
                               <@@
                                   if (%%arg: System.Nullable<float>).HasValue then
                                       [| (name, JsonValue.Float((%%arg: System.Nullable<float>).Value)) |]
                                   else
                                       [||]
                               @@>
                           | (JsonObjectType.Object, true) -> <@@ [| (name, (%%arg: NullableJsonValue).JsonVal) |] @@>
                           | (JsonObjectType.Object, false) ->
                               <@@
                                   match %%arg: NullableJsonValue with
                                   | null -> [||]
                                   | jVal -> [| (name, jVal.JsonVal) |]
                               @@>
                           | (jsonObjectType, _) -> failwithf "Unsupported type %O" jsonObjectType) ]

            ProvidedMethod(
                methodName = "Create",
                parameters = (parametersForCreate |> List.map (fun (p, _, _) -> p)),
                returnType = ty,
                isStatic = true,
                invokeCode =
                    fun args ->
                        let schemaSource = schema.ToJson()
                        let schemaHashCode = schemaSource.GetHashCode()

                        <@@
                            let record =
                                NullableJsonValue(
                                    JsonValue.Record(
                                        Array.concat (
                                            (%%(Quotations.Expr.NewArray(
                                                typeof<(string * JsonValue)[]>,
                                                processArgs args
                                            )))
                                            : (string * JsonValue)[][]
                                        )
                                    )
                                )

                            let recordSource = record.ToString()

                            let schema = SchemaCache.retrieveSchema schemaHashCode schemaSource

                            let validationErrors = schema.Validate(recordSource)

                            if Seq.isEmpty validationErrors then
                                record
                            else
                                let message =
                                    validationErrors
                                    |> Seq.map (fun validationError -> validationError.ToString())
                                    |> fun msgs ->
                                        System.String.Join(", ", msgs) |> sprintf "JSON Schema validation failed: %s"

                                raise (System.ArgumentException(message, recordSource))
                        @@>
            )

        ty.AddMember(create)
        ty

    do
        jsonSchemaTy.DefineStaticParameters(
            parameters = staticParams,
            instantiationFunction =
                fun typeName parameterValues ->
                    match parameterValues with
                    | [| :? string as schemaSource; :? string as schemaFile |] ->
                        if schemaSource = "" && schemaFile = "" || schemaSource <> "" && schemaFile <> "" then
                            failwith "Only one of schema or schemaFile must be set."

                        let schemaString =
                            if schemaSource <> "" then
                                schemaSource
                            else
                                File.ReadAllText(schemaFile)

                        let schema = SchemaCache.parseSchema schemaString
                        let schemaHashCode = schemaString.GetHashCode()

                        let ty =
                            ProvidedTypeDefinition(thisAssembly, namespaceName, typeName, baseType = Some baseTy)

                        if schema.Type <> JsonObjectType.Object then
                            failwith "Only object supported"

                        generatePropertiesAndCreateForObject ty schema |> ignore

                        let parse =
                            ProvidedMethod(
                                methodName = "Parse",
                                parameters = [ ProvidedParameter("json", typeof<string>) ],
                                returnType = ty,
                                isStatic = true,
                                invokeCode =
                                    fun args ->
                                        <@@
                                            let schema = SchemaCache.retrieveSchema schemaHashCode schemaString

                                            let validationErrors = schema.Validate((%%args[0]): string)

                                            if Seq.isEmpty validationErrors then
                                                NullableJsonValue(JsonValue.Parse(%%args[0]))
                                            else
                                                let message =
                                                    validationErrors
                                                    |> Seq.map (fun validationError -> validationError.ToString())
                                                    |> fun msgs ->
                                                        System.String.Join(", ", msgs)
                                                        |> sprintf "JSON Schema validation failed: %s"

                                                raise (System.ArgumentException(message, ((%%args[0]): string)))
                                        @@>
                            )

                        ty.AddMember(parse)

                        ty
                    | paramValues -> failwithf "Unexpected parameter values %O" paramValues
        )

    do this.AddNamespace(namespaceName, [ jsonSchemaTy ])
