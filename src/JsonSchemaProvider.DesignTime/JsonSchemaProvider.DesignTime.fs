// Copyright (c) 2023 Florian Lorenzen

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
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Reflection
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

    let fSharpCore = typeof<List<_>>.Assembly

    let listModuleType =
        fSharpCore.GetTypes() |> Array.find (fun ty -> ty.Name = "ListModule")

    let listToArray (fromType: System.Type) =
        listModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "ToArray")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(fromType)

    let listOfArray (fromType: System.Type) =
        listModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "OfArray")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(fromType)

    let arrayModuleType =
        fSharpCore.GetTypes() |> Array.find (fun ty -> ty.Name = "ArrayModule")

    let arrayMapToJsonValue (frmoType: System.Type) =
        arrayModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "Map")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(frmoType, typeof<JsonValue>)

    let listMapFromJsonValue (toType: System.Type) =
        listModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "Map")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(typeof<JsonValue>, toType)

    let jsonValueAsArray =
        match <@@ JsonValue.Array([||]).AsArray() @@> with
        | Call(_, mi, _) -> mi
        | _ -> failwith "Unexepcted expression"

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
        (providedTypeDef: ProvidedTypeDefinition)
        (isRequired: bool)
        (propType: JsonObjectType)
        =
        match propType with
        | JsonObjectType.String -> if isRequired then typeof<string> else typeof<string option>
        | JsonObjectType.Boolean -> if isRequired then typeof<bool> else typeof<bool option>
        | JsonObjectType.Integer -> if isRequired then typeof<int> else typeof<int option>
        | JsonObjectType.Number -> if isRequired then typeof<float> else typeof<float option>
        | JsonObjectType.Array ->
            let elementTy =
                determineReturnType name item.Item item providedTypeDef true item.Type

            let returnType =
                if isRequired then
                    typedefof<_ list>
                else
                    typedefof<(_ list) option>

            returnType.MakeGenericType(elementTy)
        | JsonObjectType.Object ->
            let nestedType =
                ProvidedTypeDefinition(thisAssembly, namespaceName, name + "Obj", baseType = Some baseTy)

            generatePropertiesAndCreateForObject nestedType schema |> ignore

            providedTypeDef.AddMember(nestedType)

            if isRequired then
                nestedType
            else
                let opt = typedefof<option<_>>
                opt.MakeGenericType(nestedType)
        | _ -> failwithf "Unsupported type %O" propType

    and fromJsonRecord (isRequired: bool) (returnType: System.Type) (name: string) =
        fun (args: Expr list) ->
            let fromNullableJsonVal =
                if isRequired then
                    <@@ fun (j: NullableJsonValue) -> j.JsonVal[name] @@>
                else
                    <@@ fun (j: NullableJsonValue) -> j.JsonVal.TryGetProperty(name) @@>

            let jsonValue = Expr.Application(fromNullableJsonVal, args[0])

            let rec convertFromRequiredJsonValue (ty: System.Type) =
                if ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<_ list> then
                    let elementType = ty.GenericTypeArguments[0]
                    let jsonValueVar = Var("jsonValue", typeof<JsonValue>)
                    let convertElems = convertFromRequiredJsonValue elementType

                    Expr.Lambda(
                        jsonValueVar,
                        Expr.Call(
                            listMapFromJsonValue elementType,
                            [ convertElems
                              Expr.Call(
                                  listOfArray typeof<JsonValue>,
                                  [ Expr.Call(jsonValueAsArray, [ Expr.Var(jsonValueVar) ]) ]
                              ) ]
                        )
                    )

                elif ty = typeof<string> then
                    <@@ fun (j: JsonValue) -> j.AsString() @@>
                elif ty = typeof<int> then
                    <@@ fun (j: JsonValue) -> j.AsInteger() @@>
                elif ty = typeof<float> then
                    <@@ fun (j: JsonValue) -> j.AsFloat() @@>
                elif ty = typeof<bool> then
                    <@@ fun (j: JsonValue) -> j.AsBoolean() @@>
                else
                    <@@ fun (j: JsonValue) -> NullableJsonValue(j) @@>

            let rec convertFromOptionalJsonValue (ty: System.Type) =
                printfn "convertFromOptionalJsonValue %O" ty
                let withoutOption =
                    if ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<_ option> then
                        ty.GetGenericArguments()[0]
                    else
                        ty

                if withoutOption.IsGenericType && withoutOption.GetGenericTypeDefinition() = typedefof<_ list> then
                    failwith "optional arrays nyi"
                elif withoutOption = typeof<string> then
                    <@@ fun (o: JsonValue option) -> o |> Option.map (fun j -> j.AsString()) @@>
                elif withoutOption = typeof<int> then
                    <@@ fun (o: JsonValue option) -> o |> Option.map (fun j -> j.AsInteger()) @@>
                elif withoutOption = typeof<float> then
                    <@@ fun (o: JsonValue option) -> o |> Option.map (fun j -> j.AsFloat()) @@>
                elif withoutOption = typeof<bool> then
                    <@@ fun (o: JsonValue option) -> o |> Option.map (fun j -> j.AsBoolean()) @@>
                else
                    <@@ fun (o: JsonValue option) -> o @@>

            let convert =
                if isRequired then
                    convertFromRequiredJsonValue returnType
                else
                    convertFromOptionalJsonValue returnType

            Expr.Application(convert, jsonValue)

    and generatePropertiesAndCreateForObject (providedTypeDef: ProvidedTypeDefinition) (schema: JsonSchema) =
        let properties = schema.Properties
        let requiredProperties = schema.RequiredProperties

        let parametersForCreate =
            [ for (name, schema) in [ for property in properties -> (property.Key, property.Value) ] do
                  let propType = schema.Type
                  let isRequired = requiredProperties.Contains(name)

                  let returnType =
                      determineReturnType name schema.Item schema providedTypeDef isRequired propType

                  let property =
                      ProvidedProperty(
                          propertyName = name,
                          propertyType = returnType,
                          getterCode =
                              //   if isRequired then
                              fromJsonRecord isRequired returnType name
                      //   else
                      //       fun args -> Expr.Value("1")
                      //   match propType with
                      //   | JsonObjectType.String ->
                      //       fun args ->
                      //           <@@
                      //               let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]
                      //               jsonVal.AsString()
                      //           @@>
                      //   | JsonObjectType.Boolean ->
                      //       fun args ->
                      //           <@@
                      //               let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]
                      //               jsonVal.AsBoolean()
                      //           @@>
                      //   | JsonObjectType.Integer ->
                      //       fun args ->
                      //           <@@
                      //               let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]
                      //               jsonVal.AsInteger()
                      //           @@>
                      //   | JsonObjectType.Number ->
                      //       fun args ->
                      //           <@@
                      //               let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]
                      //               jsonVal.AsFloat()
                      //           @@>
                      //   | JsonObjectType.Array ->
                      //       match prop.Value.Item.Type with
                      //       | JsonObjectType.String ->
                      //           fun args ->
                      //               <@@
                      //                   let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]

                      //                   jsonVal.AsArray()
                      //                   |> List.ofArray
                      //                   |> List.map (fun jsonVal -> jsonVal.AsString())
                      //               @@>
                      //       | JsonObjectType.Boolean ->
                      //           fun args ->
                      //               <@@
                      //                   let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]

                      //                   jsonVal.AsArray()
                      //                   |> List.ofArray
                      //                   |> List.map (fun jsonVal -> jsonVal.AsBoolean())
                      //               @@>
                      //       | JsonObjectType.Integer ->
                      //           fun args ->
                      //               <@@
                      //                   let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]

                      //                   jsonVal.AsArray()
                      //                   |> List.ofArray
                      //                   |> List.map (fun jsonVal -> jsonVal.AsInteger())
                      //               @@>
                      //       | JsonObjectType.Number ->
                      //           fun args ->
                      //               <@@
                      //                   let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]

                      //                   jsonVal.AsArray()
                      //                   |> List.ofArray
                      //                   |> List.map (fun jsonVal -> jsonVal.AsFloat())
                      //               @@>
                      //       | JsonObjectType.Object ->
                      //           fun args ->
                      //               <@@
                      //                   let jsonVal = (%%args[0]: NullableJsonValue).JsonVal[name]
                      //                   jsonVal.AsArray() |> List.ofArray
                      //               @@>
                      //       | _ -> failwithf "Unsupported type %O" propType
                      //   | JsonObjectType.Object ->
                      //       fun args -> <@@ (%%args[0]: NullableJsonValue).JsonVal[name] @@>
                      //   | _ -> failwithf "Unsupported type %O" propType

                      // Only for debug
                      //   else
                      //       match propType with
                      //       | JsonObjectType.String ->
                      //           fun args ->
                      //               <@@
                      //                   let maybeJsonVal =
                      //                       (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                      //                   maybeJsonVal
                      //                   |> Option.map (fun (jsonVal: JsonValue) -> jsonVal.AsString())
                      //               @@>
                      //       | JsonObjectType.Boolean ->
                      //           fun args ->
                      //               <@@
                      //                   let maybeJsonVal =
                      //                       (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                      //                   maybeJsonVal
                      //                   |> Option.map (fun (jsonVal: JsonValue) -> jsonVal.AsBoolean())
                      //               @@>
                      //       | JsonObjectType.Integer ->
                      //           fun args ->
                      //               <@@
                      //                   let maybeJsonVal =
                      //                       (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                      //                   maybeJsonVal
                      //                   |> Option.map (fun (jsonVal: JsonValue) -> jsonVal.AsInteger())
                      //               @@>
                      //       | JsonObjectType.Number ->
                      //           fun args ->
                      //               <@@
                      //                   let maybeJsonVal =
                      //                       (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                      //                   maybeJsonVal |> Option.map (fun (jsonVal: JsonValue) -> jsonVal.AsFloat())
                      //               @@>
                      //       | JsonObjectType.Array ->
                      //           // TODO: Use Expr to and build a proper conversion from the array's element type that also allows nested types
                      //           match prop.Value.Item.Type with
                      //           | JsonObjectType.String ->
                      //               fun args ->
                      //                   <@@
                      //                       let maybeJsonVal =
                      //                           (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                      //                       match maybeJsonVal with
                      //                       | None -> List.empty
                      //                       | Some(jsonVal) ->
                      //                           jsonVal.AsArray()
                      //                           |> List.ofArray
                      //                           |> List.map (fun jsonVal -> jsonVal.AsString())
                      //                   @@>
                      //           | JsonObjectType.Boolean ->
                      //               fun args ->
                      //                   <@@
                      //                       let maybeJsonVal =
                      //                           (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                      //                       match maybeJsonVal with
                      //                       | None -> List.empty
                      //                       | Some(jsonVal) ->
                      //                           jsonVal.AsArray()
                      //                           |> List.ofArray
                      //                           |> List.map (fun jsonVal -> jsonVal.AsBoolean())
                      //                   @@>
                      //           | JsonObjectType.Integer ->
                      //               fun args ->
                      //                   <@@
                      //                       let maybeJsonVal =
                      //                           (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                      //                       match maybeJsonVal with
                      //                       | None -> List.empty
                      //                       | Some(jsonVal) ->
                      //                           jsonVal.AsArray()
                      //                           |> List.ofArray
                      //                           |> List.map (fun jsonVal -> jsonVal.AsInteger())
                      //                   @@>
                      //           | JsonObjectType.Number ->
                      //               fun args ->
                      //                   <@@
                      //                       let maybeJsonVal =
                      //                           (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                      //                       match maybeJsonVal with
                      //                       | None -> List.empty
                      //                       | Some(jsonVal) ->
                      //                           jsonVal.AsArray()
                      //                           |> List.ofArray
                      //                           |> List.map (fun jsonVal -> jsonVal.AsFloat())
                      //                   @@>
                      //           | JsonObjectType.Object ->
                      //               fun args ->
                      //                   <@@
                      //                       let maybeJsonVal =
                      //                           (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                      //                       match maybeJsonVal with
                      //                       | None -> List.empty
                      //                       | Some(jsonVal) -> jsonVal.AsArray() |> List.ofArray

                      //                   @@>
                      //           | _ -> failwithf "Unsupported type %O" propType
                      //       | JsonObjectType.Object ->
                      //           fun args -> <@@ (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name) @@>
                      //       | _ -> failwithf "Unsupported type %O" propType
                      )

                  providedTypeDef.AddMember(property)

                  yield
                      if isRequired then
                          (ProvidedParameter(name, returnType, false), propType, schema, isRequired)
                      else
                          let returnTypeWithoutOption = returnType.GenericTypeArguments[0]

                          let (nullableReturnType, defaultValue) =
                              if returnTypeWithoutOption.IsValueType then
                                  ((typedefof<System.Nullable<_>>).MakeGenericType([| returnTypeWithoutOption |]),
                                   System.Nullable() :> obj)
                              else
                                  (returnTypeWithoutOption, null :> obj)

                          (ProvidedParameter(name, nullableReturnType, false, defaultValue),
                           propType,
                           schema,
                           isRequired) ]

        let create =
            let processArgs (args: Quotations.Expr list) =
                let rec toJsonValue (parameterType: System.Type) =
                    if
                        parameterType.IsGenericType
                        && parameterType.GetGenericTypeDefinition() = typedefof<_ list>
                    then
                        let elemType = parameterType.GetGenericArguments()[0]

                        let jsonValueArray =
                            FSharpType.GetUnionCases(typeof<JsonValue>)
                            |> Array.find (fun uc -> uc.Name = "Array")

                        let typedObjVar = Var("obj", parameterType)

                        Expr.Lambda(
                            typedObjVar,
                            Expr.NewUnionCase(
                                jsonValueArray,
                                [ Expr.Call(
                                      arrayMapToJsonValue elemType,
                                      [ toJsonValue elemType
                                        Expr.Call(listToArray elemType, [ Expr.Var(typedObjVar) ]) ]
                                  ) ]
                            )
                        )
                    elif parameterType = typeof<string> then
                        <@@ fun (x: string) -> JsonValue.String(x) @@>
                    elif parameterType = typeof<int> then
                        <@@ fun (x: int) -> JsonValue.Number(decimal x) @@>
                    elif parameterType = typeof<float> then
                        <@@ fun (x: float) -> JsonValue.Float(x) @@>
                    elif parameterType = typeof<bool> then
                        <@@ fun (x: bool) -> JsonValue.Boolean(x) @@>
                    else
                        <@@ fun (x: NullableJsonValue) -> x.JsonVal @@>

                let requiredToRecordArg (parameterName: string) (parameterType: System.Type) (arg: Expr) =
               
                    let convert = toJsonValue parameterType

                    Expr.NewArray(
                        typeof<string * JsonValue>,
                        [ Expr.NewTuple([ <@@ parameterName @@>; Expr.Application(convert, arg) ]) ]
                    )

                let optionalToRecordArg (parameterName: string) (parameterType: System.Type) (arg: Expr) =
                    printfn "paremeterType %O" parameterType
                    let withoutNullable =
                        if parameterType.IsGenericType && parameterType.GetGenericTypeDefinition() = typedefof<System.Nullable<_>> then
                            parameterType.GetGenericArguments()[0]
                        else
                            parameterType
                    let convert = toJsonValue withoutNullable
                    printfn "without nullable %O" withoutNullable

                    let hasValuePropertyInfo: PropertyInfo =
                        match <@@ System.Nullable(0).HasValue @@> with
                        | Let(_, _, PropertyGet(_, pi, _)) -> pi
                        | x -> failwithf "unexpected expr %O" x

                    let valuePropertyInfo: PropertyInfo =
                        match <@@ System.Nullable(0).Value @@> with
                        | Let(_, _, PropertyGet(_, pi, _)) -> pi
                        | x -> failwithf "unexpected expr %O" x

                    if
                        withoutNullable.IsGenericType
                        && withoutNullable.GetGenericTypeDefinition() = typedefof<_ list>
                    then
                        failwith ""
                    elif withoutNullable = typeof<string> then
                        failwith ""
                    elif withoutNullable = typeof<int> then
                        printfn "111"
                        let expr =
                            Expr.IfThenElse(
                                Expr.PropertyGet(arg, hasValuePropertyInfo),
                                Expr.NewArray(
                                    typeof<string * JsonValue>,
                                    [ Expr.NewTuple(
                                          [ <@@ parameterName @@>
                                            Expr.Application(convert, Expr.PropertyGet(arg, valuePropertyInfo)) ]
                                      ) ]
                                ),
                                Expr.NewArray(typeof<string * JsonValue>, [])
                            )

                        printfn "expr %O " expr
                        expr
                    // <@@ if arg.HasValue then
                    //       [| (parameterName, convert (arg.Value) ) |]
                    //     else
                    //       [||]
                    // @@>
                    // Expr.Let(Var("x", typeof<System.Nullable<int>>), Expr.Application(convert, arg), Expr.IfThenElse(Expr.Var()))
                    // <@@
                    //  if (%%arg: System.Nullable<int>).HasValue then
                    //    [| (parameterName, JsonValue.Float((%%arg: System.Nullable<int>).Value)) |]
                    //  else
                    //    [||]
                    // @@>
                    // failwith ""
                    elif withoutNullable = typeof<float> then
                        failwith "x float"
                    elif withoutNullable = typeof<bool> then
                        failwith "x bool"
                    else
                        failwith "x other"


                [ for (arg, (parameter, propType, propValue, isRequired)) in List.zip args parametersForCreate do
                      yield
                          if isRequired then
                              requiredToRecordArg (parameter.Name) (parameter.ParameterType) arg
                          else
                              optionalToRecordArg (parameter.Name) (parameter.ParameterType) arg ]
            //   (match (propType, isRequired) with
            //    | (_, true) ->
            //    | (JsonObjectType.String, true) ->
            //        let ucString =
            //            FSharpType.GetUnionCases(typeof<JsonValue>)
            //            |> Array.find (fun uc -> uc.Name = "String")

            //        Expr.NewArray(
            //            typeof<string * JsonValue>,
            //            [ Expr.NewTuple([ <@@ name @@>; Expr.NewUnionCase(ucString, [ arg ]) ]) ]
            //        )
            //    //<@@ [| (name, JsonValue.String(%%arg: string)) |] @@>
            //    | (JsonObjectType.String, false) ->
            //        <@@
            //            match %%arg: string with
            //            | null -> [||]
            //            | value -> [| (name, JsonValue.String(value)) |]
            //        @@>
            //    | (JsonObjectType.Integer, true) ->
            //        <@@ [| (name, JsonValue.Float((%%arg: int) |> float)) |] @@>
            //    | (JsonObjectType.Integer, false) ->
            //        <@@
            //            if (%%arg: System.Nullable<int>).HasValue then
            //                [| (name, JsonValue.Float((%%arg: System.Nullable<int>).Value)) |]
            //            else
            //                [||]
            //        @@>
            //    | (JsonObjectType.Number, true) -> <@@ [| (name, JsonValue.Float(%%arg: float)) |] @@>
            //    | (JsonObjectType.Number, false) ->
            //        <@@
            //            if (%%arg: System.Nullable<float>).HasValue then
            //                [| (name, JsonValue.Float((%%arg: System.Nullable<float>).Value)) |]
            //            else
            //                [||]
            //        @@>
            //    | (JsonObjectType.Array, true) ->

            //        let (convertToJsonValue, toType) =
            //            match propValue.Item.Type with
            //            | JsonObjectType.Number -> (<@@ fun num -> JsonValue.Float(num) @@>, typeof<float>) // TODO: build proper conversion from the array's element type that also allows nesting
            //            | JsonObjectType.Integer -> (<@@ fun num -> JsonValue.Number(num) @@>, typeof<int>)

            //        let ucArray =
            //            FSharpType.GetUnionCases(typeof<JsonValue>)
            //            |> Array.find (fun uc -> uc.Name = "Array")

            //        let fSharpCore = typeof<List<_>>.Assembly

            //        let listModuleType =
            //            fSharpCore.GetTypes() |> Array.find (fun ty -> ty.Name = "ListModule")

            //        let miToArray =
            //            listModuleType.GetMethods()
            //            |> Array.find (fun methodInfo -> methodInfo.Name = "ToArray")
            //            |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(toType)

            //        let arrayModuleType =
            //            fSharpCore.GetTypes() |> Array.find (fun ty -> ty.Name = "ArrayModule")

            //        let miMap =
            //            arrayModuleType.GetMethods()
            //            |> Array.find (fun methodInfo -> methodInfo.Name = "Map")
            //            |> fun genericMethodInfo ->
            //                genericMethodInfo.MakeGenericMethod(toType, typeof<JsonValue>)

            //        Expr.NewArray(
            //            typeof<string * JsonValue>,
            //            [ Expr.NewTuple(
            //                  [ <@@ name @@>
            //                    Expr.NewUnionCase(
            //                        ucArray,
            //                        [ Expr.Call(miMap, [ convertToJsonValue; Expr.Call(miToArray, [ arg ]) ]) ]
            //                    ) ]
            //              ) ]
            //        )
            //    // this works:
            //    //    <@@
            //    //        [| (name, JsonValue.Array((%%arg: List<float>) |> List.toArray |> Array.map (%%conv))) |]
            //    //    @@>

            //    | (JsonObjectType.Object, true) -> <@@ [| (name, (%%arg: NullableJsonValue).JsonVal) |] @@>
            //    | (JsonObjectType.Object, false) ->
            //        <@@
            //            match %%arg: NullableJsonValue with
            //            | null -> [||]
            //            | jVal -> [| (name, jVal.JsonVal) |]
            //        @@>
            //    | (jsonObjectType, _) -> failwithf "Unsupported type %O" jsonObjectType) ]

            ProvidedMethod(
                methodName = "Create",
                parameters = (parametersForCreate |> List.map (fun (p, _, _, _) -> p)),
                returnType = providedTypeDef,
                isStatic = true,
                invokeCode =
                    fun args ->
                        let schemaSource = schema.ToJson()
                        let schemaHashCode = schemaSource.GetHashCode()
                        let arrays = processArgs args
                        printfn "args %O" arrays

                        <@@
                            let record =
                                NullableJsonValue(
                                    JsonValue.Record(
                                        Array.concat (
                                            (%%(Expr.NewArray(typeof<(string * JsonValue)[]>, arrays)))
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

        providedTypeDef.AddMember(create)
        providedTypeDef

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
