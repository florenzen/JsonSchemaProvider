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

module TypeProvider =
    open System
    open System.Reflection
    open SchemaRep
    open ProviderImplementation.ProvidedTypes
    open NJsonSchema

    type private ProvidedTypeData =
        { Assembly: Assembly
          NamespaceName: string
          RuntimeType: Type }

    let rec private fSharpTypeToDotnetType
        (classMap: Map<string, ProvidedTypeDefinition>)
        (fSharpType: FSharpType)
        : Type =
        match fSharpType with
        | FSharpBool -> typeof<bool>
        | FSharpClass(name) -> classMap[name]
        | FSharpList(innerFSharpType) ->
            let innerDotnetType = fSharpTypeToDotnetType classMap innerFSharpType
            typedefof<_ list>.MakeGenericType(innerDotnetType)
        | FSharpDouble -> typeof<double>
        | FSharpInt -> typeof<int>
        | FSharpString -> typeof<string>

    let private optionalOrPlainType (optional: bool) (dotnetType: Type) : Type =
        if optional then
            typedefof<_ option>.MakeGenericType(dotnetType)
        else
            dotnetType

    let rec private fSharpTypeToPropertyType
        (classMap: Map<string, ProvidedTypeDefinition>)
        (optional: bool)
        (fSharpType: FSharpType)
        : Type =
        let dotnetType = fSharpTypeToDotnetType classMap fSharpType
        optionalOrPlainType optional dotnetType

    let private nullableOrPlainType (optional: bool) (dotnetType: Type) : Type =
        if optional then
            let dotnetTypeWithoutOption = dotnetType.GenericTypeArguments[0]

            if dotnetTypeWithoutOption.IsValueType then
                typedefof<Nullable<_>>.MakeGenericType(dotnetTypeWithoutOption) //, Some(Nullable() :> obj))
            else
                dotnetTypeWithoutOption //, Some(null :> obj)
        else
            dotnetType

    let private defaultValueForNullableType (dotnetType: Type) : obj =
        let dotnetTypeWithoutOption = dotnetType.GenericTypeArguments[0]

        if dotnetTypeWithoutOption.IsValueType then
            Nullable()
        else
            null

    let rec private fSharpTypeToMethodParameterType
        (classMap: Map<string, ProvidedTypeDefinition>)
        (optional: bool)
        (fSharpType: FSharpType)
        : Type =
        let dotnetType = fSharpTypeToDotnetType classMap fSharpType
        nullableOrPlainType optional dotnetType

    let private createProvidedProperties
        (classMap: Map<string, ProvidedTypeDefinition>)
        (properties: FSharpProperty list)
        : ProvidedProperty list =
        [ for { Name = name
                Optional = optional
                FSharpType = fSharpType } in properties ->

              ProvidedProperty(
                  name,
                  fSharpTypeToPropertyType classMap optional fSharpType,
                  ?getterCode = None // TODO
              ) ]

    let private createProvidedCreateMethod
        (classMap: Map<string, ProvidedTypeDefinition>)
        (properties: FSharpProperty list)
        (providedTypeDefinition: ProvidedTypeDefinition)
        : ProvidedMethod =
        let parameters =
            [ for property in properties do
                  let parameterType =
                      fSharpTypeToMethodParameterType classMap (property.Optional) property.FSharpType

                  if property.Optional then
                      ProvidedParameter(property.Name, parameterType, false, defaultValueForNullableType parameterType)
                  else
                      ProvidedParameter(property.Name, parameterType) ]

        ProvidedMethod("Create", parameters, providedTypeDefinition, ?invokeCode = None)

    let rec private createSubClassProvidedTypeDefinitions
        (providedTypeData: ProvidedTypeData)
        (subClasses: FSharpClassTree list)
        : Map<string, ProvidedTypeDefinition> =
        subClasses
        |> List.map (fun subClass -> (subClass.Name, fSharpClassTreeToProvidedTypeDefinition providedTypeData subClass))
        |> Map.ofList

    and private fSharpClassTreeToProvidedTypeDefinition
        (providedTypeData: ProvidedTypeData)
        { Name = className
          Properties = properties
          SubClasses = subClasses }
        : ProvidedTypeDefinition =
        let providedTypeDefinition =
            ProvidedTypeDefinition(
                providedTypeData.Assembly,
                providedTypeData.NamespaceName,
                className,
                Some(providedTypeData.RuntimeType)
            )

        let classMap = createSubClassProvidedTypeDefinitions providedTypeData subClasses

        classMap
        |> Map.values
        |> Seq.iter (fun subClassProvidedTypeDefinition ->
            providedTypeDefinition.AddMember(subClassProvidedTypeDefinition))

        let providedProperties = createProvidedProperties classMap properties

        providedProperties
        |> List.iter (fun providedProperty -> providedTypeDefinition.AddMember(providedProperty))

        let createMethod =
            createProvidedCreateMethod classMap properties providedTypeDefinition

        providedTypeDefinition.AddMember(createMethod)

        providedTypeDefinition

    let run
        (schema: JsonSchema)
        (schemaHashCode: int32)
        (assembly: Assembly)
        (namespaceName: string)
        (typeName: string)
        (runtimeType: Type)
        : ProvidedTypeDefinition =
        let providedTypeData =
            { Assembly = assembly
              NamespaceName = namespaceName
              RuntimeType = runtimeType }

        let fSharpClassTree =
            parseJsonSchemaStructured schema |> jsonObjectToFSharpClassTree typeName

        fSharpClassTreeToProvidedTypeDefinition providedTypeData fSharpClassTree
// TODO: parse method
