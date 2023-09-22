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

    let rec generateSelectorsForObject
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

                    generateSelectorsForObject innerTy prop.Value.Properties |> ignore
                    ty.AddMember(innerTy)

                    if requiredProps.Contains(name) then innerTy else innerTy
                | _ -> failwith $"Unsupported type {propType}"

            let accessor =
                ProvidedProperty(
                    propertyName = name,
                    propertyType = returnType,
                    getterCode =
                        fun args ->
                            <@@
                                let jval = (%%args[0]: JsonValue).TryGetProperty(name)
                                let unwrap jval = if isRequired then (Option.get jval):>obj else jval:>obj
                                match propType with
                                | JsonObjectType.String -> Option.map (fun (x:JsonValue )-> x.AsString()) jval |> unwrap
                                // | JsonObjectType.Boolean -> Option.map (fun x -> x.AsBoolean():>obj) jval |> unwrap
                                // | JsonObjectType.Integer -> Option.map (fun x -> x.AsInteger():>obj) jval |> unwrap
                                // | JsonObjectType.Number -> Option.map (fun x -> x.AsFloat():>obj) jval |> unwrap
                                // | JsonObjectType.Object -> jval: obj
                                | _ -> failwith $"Unsupported type {propType}"
                            @@>
                )

            ty.AddMember(accessor)

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

                        generateSelectorsForObject ty schema.Properties schema.RequiredProperties|> ignore

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
                    | _ -> failwith "unexpected paramter values"
        )

    do this.AddNamespace(namespaceName, [ jsonSchemaTy ])


[<assembly: TypeProviderAssembly>]
do ()

// https://medium.com/@haumohio/the-trips-and-traps-of-creating-a-generative-type-provider-in-f-75162d99622c
// http://blog.mavnn.co.uk/type-providers-from-the-ground-up/
// https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/383
