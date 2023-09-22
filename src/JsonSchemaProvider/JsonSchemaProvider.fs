namespace JsonSchemaProvider

open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

type PPair(fst: string, snd: string) =
    member __.Fst() = fst
    member __.Snd() = snd


[<TypeProvider>]
type JsonSchemaProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config)
    let namespaceName = "JsonSchemaProvider"
    let thisAssembly = Assembly.GetExecutingAssembly()
    let staticParams = [ ProvidedStaticParameter("prefix", typeof<string>) ]
    let baseTy = typeof<PPair>

    let jsonTy =
        ProvidedTypeDefinition(thisAssembly, namespaceName, "Pair", baseType = Some baseTy)

    do
        jsonTy.DefineStaticParameters(
            parameters = staticParams,
            instantiationFunction =
                fun typeName parameterValues ->
                    match parameterValues with
                    | [| :? string as prefix |] ->
                        let ty =
                            ProvidedTypeDefinition(thisAssembly, namespaceName, typeName, baseType = Some baseTy)

                        let fst =
                            ProvidedMethod(
                                methodName = prefix + "Fst",
                                parameters = [],
                                returnType = typeof<string>,
                                isStatic = false,
                                invokeCode = fun args -> <@@ ((%%args[0]: PPair)).Fst() @@>
                            )

                        let snd =
                            ProvidedMethod(
                                methodName = prefix + "Snd",
                                parameters = [],
                                returnType = typeof<string>,
                                isStatic = false,
                                invokeCode = fun args -> <@@ ((%%args[0]: PPair)).Snd() @@>
                            )

                        ty.AddMember(fst)
                        ty.AddMember(snd)

                        let ctor =
                            ProvidedConstructor(
                                parameters =
                                    [ ProvidedParameter("fst", typeof<string>)
                                      ProvidedParameter("snd", typeof<string>) ],
                                invokeCode =
                                    fun args ->
                                        <@@ PPair((%%(args[0]): string), (%%(args[1]): string)) @@>
                            )

                        ty.AddMember(ctor)
                        ty
                    | _ -> failwith "unexpected paramter values"
        )

    do this.AddNamespace(namespaceName, [ jsonTy ])

[<assembly: TypeProviderAssembly>]
do ()

// https://medium.com/@haumohio/the-trips-and-traps-of-creating-a-generative-type-provider-in-f-75162d99622c
// http://blog.mavnn.co.uk/type-providers-from-the-ground-up/
// https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/383
