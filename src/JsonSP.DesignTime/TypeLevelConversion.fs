// Copyright (c) 2024 Florian Lorenzen

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

module TypeLevelConversion =
    open System
    open SchemaConversion
    open ProviderImplementation.ProvidedTypes
    open JsonSchemaProvider

    let rec fSharpTypeToCompileTimeType
        (classMap: Map<string, ProvidedTypeDefinition>)
        (fSharpType: FSharpType)
        : Type =
        match fSharpType with
        | FSharpBool -> typeof<bool>
        | FSharpClass(name) -> classMap[name]
        | FSharpList(innerFSharpType) ->
            let innerStaticType = fSharpTypeToCompileTimeType classMap innerFSharpType
            typedefof<_ list>.MakeGenericType(innerStaticType)
        | FSharpDouble -> typeof<double>
        | FSharpInt -> typeof<int>
        | FSharpString -> typeof<string>

    let rec fSharpTypeToRuntimeType (classMap: Map<string, ProvidedTypeDefinition>) (fSharpType: FSharpType) : Type =
        match fSharpType with
        | FSharpBool -> typeof<bool>
        | FSharpClass(_) -> typeof<NullableJsonValue>
        | FSharpList(innerFSharpType) ->
            let innerRuntimeType = fSharpTypeToRuntimeType classMap innerFSharpType
            typedefof<_ list>.MakeGenericType(innerRuntimeType)
        | FSharpDouble -> typeof<double>
        | FSharpInt -> typeof<int>
        | FSharpString -> typeof<string>

    let optionalOrPlainType (optional: bool) (dotnetType: Type) : Type =
        if optional then
            typedefof<_ option>.MakeGenericType(dotnetType)
        else
            dotnetType

    let nullableOrPlainType (optional: bool) (dotnetType: Type) : Type =
        if optional then
            let dotnetTypeWithoutOption = dotnetType.GenericTypeArguments[0]

            if dotnetTypeWithoutOption.IsValueType then
                typedefof<Nullable<_>>.MakeGenericType(dotnetTypeWithoutOption)
            else
                dotnetTypeWithoutOption
        else
            dotnetType

    let defaultValueForNullableType (dotnetType: Type) : obj =
        let dotnetTypeWithoutOption = dotnetType.GenericTypeArguments[0]

        if dotnetTypeWithoutOption.IsValueType then
            Nullable()
        else
            null

    let rec fSharpTypeToMethodParameterType
        (classMap: Map<string, ProvidedTypeDefinition>)
        (optional: bool)
        (fSharpType: FSharpType)
        : Type =
        let dotnetType = fSharpTypeToCompileTimeType classMap fSharpType
        nullableOrPlainType optional dotnetType
