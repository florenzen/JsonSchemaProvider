namespace JsonSchemaProvider

open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open NJsonSchema
open FSharp.Data

[<AllowNullLiteral>]
type NullableJsonValue(jsonVal: JsonValue) =
    member val JsonVal = jsonVal
    override this.ToString(): string = this.JsonVal.ToString()

[<TypeProvider>]
type JsonSchemaProviderImpl(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config)
    let namespaceName = "JsonSchemaProvider"
    let thisAssembly = Assembly.GetExecutingAssembly()
    let staticParams = [ ProvidedStaticParameter("schema", typeof<string>) ]
    let baseTy = typeof<NullableJsonValue>

    let jsonSchemaTy =
        ProvidedTypeDefinition(thisAssembly, namespaceName, "JsonSchemaProvider", baseType = Some baseTy)

    let rec determineReturnType
        (name: string)
        (item: JsonSchema)
        (schema: JsonSchema)
        // (properties: IDictionary<string, JsonSchemaProperty>)
        // (requiredProperties: ICollection<string>)
        (ty: ProvidedTypeDefinition)
        (isRequired: bool)
        (propType: JsonObjectType)
        =
        printfn "DET RET TYPE FOR %s %O" name schema

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

            generatePropertiesAndCreateForObject innerTy schema //properties requiredProperties
            |> ignore

            printf "Add %O to %O" innerTy ty
            ty.AddMember(innerTy)

            if isRequired then
                innerTy
            else
                let opt = typedefof<option<_>>
                opt.MakeGenericType(innerTy)
        | _ -> failwithf "Unsupported type %O" propType

    and generatePropertiesAndCreateForObject
        (ty: ProvidedTypeDefinition)
        (schema: JsonSchema)
        // (properties: IDictionary<string, JsonSchemaProperty>)
        // (requiredProperties: ICollection<string>)
        =
        printfn "%O" (schema)
        let properties = schema.Properties
        let requiredProperties = schema.RequiredProperties

        let parametersForCreate =
            [ for prop in properties do
                  let name = prop.Key
                  let propType = prop.Value.Type
                  let isRequired = requiredProperties.Contains(name)

                  let returnType =
                      determineReturnType
                          name
                          prop.Value.Item
                          prop.Value
                          // prop.Value.Properties
                          // prop.Value.RequiredProperties
                          ty
                          isRequired
                          propType

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
                                  | JsonObjectType.Object -> fun args -> <@@ (%%args[0]: NullableJsonValue).JsonVal[name] @@>
                                  | _ -> failwithf "Unsupported type %O" propType
                              else
                                  match propType with
                                  | JsonObjectType.String ->
                                      fun args ->
                                          <@@
                                              let maybeJsonVal = (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                                              maybeJsonVal
                                              |> Option.map (fun (jsonVal: JsonValue) -> jsonVal.AsString())
                                          @@>
                                  | JsonObjectType.Boolean ->
                                      fun args ->
                                          <@@
                                              let maybeJsonVal = (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                                              maybeJsonVal
                                              |> Option.map (fun (jsonVal: JsonValue) -> jsonVal.AsBoolean())
                                          @@>
                                  | JsonObjectType.Integer ->
                                      fun args ->
                                          <@@
                                              let maybeJsonVal = (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

                                              maybeJsonVal
                                              |> Option.map (fun (jsonVal: JsonValue) -> jsonVal.AsInteger())
                                          @@>
                                  | JsonObjectType.Number ->
                                      fun args ->
                                          <@@
                                              let maybeJsonVal = (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)
                                              maybeJsonVal |> Option.map (fun (jsonVal: JsonValue) -> jsonVal.AsFloat())
                                          @@>
                                  | JsonObjectType.Array ->
                                      match prop.Value.Item.Type with
                                      | JsonObjectType.String ->
                                          fun args ->
                                              <@@
                                                  let maybeJsonVal = (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

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
                                                  let maybeJsonVal = (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

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
                                                  let maybeJsonVal = (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

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
                                                  let maybeJsonVal = (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

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
                                                  let maybeJsonVal = (%%args[0]: NullableJsonValue).JsonVal.TryGetProperty(name)

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

        for p in parametersForCreate do
            printfn "PARAM %O" p

        printfn ""
        printfn ""
        
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
                           | (JsonObjectType.Object, true) -> <@@ [| (name, %%arg) |] @@>
                           | (JsonObjectType.Object, false) ->
                                <@@ 
                                    match %%arg:NullableJsonValue with | null -> [||] | jVal -> [|(name, jVal.JsonVal)|]
                                @@>
                           | (jsonObjectType, _) -> failwithf "Unsupported type %O" jsonObjectType) ]

            ProvidedMethod(
                methodName = "Create",
                parameters = (parametersForCreate |> List.map (fun (p, _, _) -> p)),
                returnType = ty,
                isStatic = true,
                invokeCode =
                    fun args ->
                        <@@
                            let record =
                                NullableJsonValue(JsonValue.Record(
                                    Array.concat (
                                        (%%(Quotations.Expr.NewArray(typeof<(string * JsonValue)[]>, processArgs args)))
                                        : (string * JsonValue)[][]
                                    )
                                ))

                            record
                        // schema
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
                    | [| :? string as schemaSource |] ->
                        let schema =
                            JsonSchema.FromJsonAsync(schemaSource)
                            |> Async.AwaitTask
                            |> Async.RunSynchronously

                        let ty =
                            ProvidedTypeDefinition(thisAssembly, namespaceName, typeName, baseType = Some baseTy)

                        if schema.Type <> JsonObjectType.Object then
                            failwith "Only object supported"

                        generatePropertiesAndCreateForObject ty schema //schema.Properties schema.RequiredProperties
                        |> ignore

                        let parse =
                            ProvidedMethod(
                                methodName = "Parse",
                                parameters = [ ProvidedParameter("json", typeof<string>) ],
                                returnType = ty,
                                isStatic = true,
                                invokeCode =
                                    fun args ->
                                        <@@
                                            let schema =
                                                JsonSchema.FromJsonAsync(schemaSource)
                                                |> Async.AwaitTask
                                                |> Async.RunSynchronously

                                            let result = schema.Validate((%%args[0]): string)

                                            if Seq.isEmpty result then
                                                NullableJsonValue(JsonValue.Parse(%%args[0]))
                                            else
                                                failwith "Invalid JSON"
                                        @@>
                            )

                        ty.AddMember(parse)

                        ty
                    | paramValues -> failwithf "Unexpected parameter values %O" paramValues
        )

    do this.AddNamespace(namespaceName, [ jsonSchemaTy ])


[<assembly: TypeProviderAssembly>]
do ()

// https://medium.com/@haumohio/the-trips-and-traps-of-creating-a-generative-type-provider-in-f-75162d99622c
// http://blog.mavnn.co.uk/type-providers-from-the-ground-up/
// https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/383
//
// TODO
// - automatic tests
// - builder functions
