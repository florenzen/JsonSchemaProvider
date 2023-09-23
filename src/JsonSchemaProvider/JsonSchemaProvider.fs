namespace JsonSchemaProvider

open System.Collections.Generic
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open NJsonSchema
open FSharp.Data
open FSharp.Data.JsonExtensions

[<TypeProvider>]
type JsonSchemaProviderImpl(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config)
    let namespaceName = "JsonSchemaProvider"
    let thisAssembly = Assembly.GetExecutingAssembly()
    let staticParams = [ ProvidedStaticParameter("schema", typeof<string>) ]
    let baseTy = typeof<JsonValue>

    let jsonSchemaTy =
        ProvidedTypeDefinition(thisAssembly, namespaceName, "JsonSchemaProvider", baseType = Some baseTy)

    let rec generatePropertiesForObject
        (ty: ProvidedTypeDefinition)
        (props: IDictionary<string, JsonSchemaProperty>)
        (requiredProps: ICollection<string>)
        =
        for prop in props do
            let name = prop.Key
            let propType = prop.Value.Type
            let isRequired = requiredProps.Contains(name)

            let returnType =
                match propType with
                | JsonObjectType.String -> if isRequired then typeof<string> else typeof<string option>
                | JsonObjectType.Boolean -> if isRequired then typeof<bool> else typeof<bool option>
                | JsonObjectType.Integer -> if isRequired then typeof<int> else typeof<int option>
                | JsonObjectType.Number -> if isRequired then typeof<float> else typeof<float option>
                | JsonObjectType.Object ->
                    let innerTy =
                        ProvidedTypeDefinition(thisAssembly, namespaceName, name, baseType = Some baseTy)

                    generatePropertiesForObject innerTy prop.Value.Properties prop.Value.RequiredProperties
                    |> ignore

                    ty.AddMember(innerTy)

                    if isRequired then
                        innerTy
                    else
                        let opt = typedefof<option<_>>
                        opt.MakeGenericType(innerTy)
                | _ -> failwith $"Unsupported type {propType}"

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
                                        let jsonVal = (%%args[0]: JsonValue)[name]
                                        jsonVal.AsString()
                                    @@>
                            | JsonObjectType.Boolean ->
                                fun args ->
                                    <@@
                                        let jsonVal = (%%args[0]: JsonValue)[name]
                                        jsonVal.AsBoolean()
                                    @@>
                            | JsonObjectType.Integer ->
                                fun args ->
                                    <@@
                                        let jsonVal = (%%args[0]: JsonValue)[name]
                                        jsonVal.AsInteger()
                                    @@>
                            | JsonObjectType.Number ->
                                fun args ->
                                    <@@
                                        let jsonVal = (%%args[0]: JsonValue)[name]
                                        jsonVal.AsFloat()
                                    @@>
                            | JsonObjectType.Object ->
                                fun args ->
                                    <@@ (%%args[0]: JsonValue)[name]

                                    @@>
                            | _ -> failwithf "Unsupported type %O" propType
                        else
                            match propType with
                            | JsonObjectType.String ->
                                fun args ->
                                    <@@
                                        let maybeJsonVal = (%%args[0]: JsonValue).TryGetProperty(name)
                                        maybeJsonVal |> Option.map (fun jsonVal -> jsonVal.AsString())
                                    @@>
                            | JsonObjectType.Boolean ->
                                fun args ->
                                    <@@
                                        let maybeJsonVal = (%%args[0]: JsonValue).TryGetProperty(name)
                                        maybeJsonVal |> Option.map (fun jsonVal -> jsonVal.AsBoolean())
                                    @@>
                            | JsonObjectType.Integer ->
                                fun args ->
                                    <@@
                                        let maybeJsonVal = (%%args[0]: JsonValue).TryGetProperty(name)
                                        maybeJsonVal |> Option.map (fun jsonVal -> jsonVal.AsInteger())
                                    @@>
                            | JsonObjectType.Number ->
                                fun args ->
                                    <@@
                                        let maybeJsonVal = (%%args[0]: JsonValue).TryGetProperty(name)
                                        maybeJsonVal |> Option.map (fun jsonVal -> jsonVal.AsFloat())
                                    @@>
                            | JsonObjectType.Object -> fun args -> <@@ (%%args[0]: JsonValue).TryGetProperty(name) @@>
                            | _ -> failwithf "Unsupported type %O" propType
                )

            ty.AddMember(property)

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

                        generatePropertiesForObject ty schema.Properties schema.RequiredProperties
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
                                                JsonValue.Parse(%%args[0])
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
