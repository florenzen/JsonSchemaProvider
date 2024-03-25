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

namespace JsonSP.DesignTime

open System.IO
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open JsonSchemaProvider

[<TypeProvider>]
type JsonSPImpl(config: TypeProviderConfig) as this =
    inherit
        TypeProviderForNamespaces(
            config,
            assemblyReplacementMap = [ ("JsonSP.DesignTime", "JsonSchemaProvider.Runtime") ],
            addDefaultProbingLocation = true
        )

    let namespaceName = "JsonSP"
    let thisAssembly = Assembly.GetExecutingAssembly()

    let staticParams =
        [ ProvidedStaticParameter("schema", typeof<string>, "")
          ProvidedStaticParameter("schemaFile", typeof<string>, "") ]

    let runtimeType = typeof<NullableJsonValue>

    let jsonSchemaType =
        ProvidedTypeDefinition(thisAssembly, namespaceName, "JsonSP", baseType = Some runtimeType)

    let instantiate (typeName: string) (parameterValues: obj[]) =
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

            let providedType =
                TypeProvider.run schema schemaHashCode thisAssembly namespaceName typeName runtimeType

            providedType
        | paramValues -> failwithf "Unexpected parameter values %A." paramValues

    do
        jsonSchemaType.DefineStaticParameters(parameters = staticParams, instantiationFunction = instantiate)

        this.AddNamespace(namespaceName, [ jsonSchemaType ])
