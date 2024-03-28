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
    open ExprGenerator
    open ProviderImplementation.ProvidedTypes
    open NJsonSchema
    open JsonSchemaProvider
    open FSharp.Data

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

    let private fSharpTypeToPropertyType
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
                typedefof<Nullable<_>>.MakeGenericType(dotnetTypeWithoutOption)
            else
                dotnetTypeWithoutOption
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
                FSharpType = fSharpType } as property in properties ->

              let plainPropertyType = fSharpTypeToDotnetType classMap fSharpType

              ProvidedProperty(
                  propertyName = name,
                  propertyType = optionalOrPlainType optional plainPropertyType,
                  getterCode = generatePropertyGetter plainPropertyType property
              ) ]

    let private createProvidedCreateMethod
        (classMap: Map<string, ProvidedTypeDefinition>)
        (properties: FSharpProperty list)
        (schemaHashCode: int32)
        (schemaString: string)
        (providedTypeDefinition: ProvidedTypeDefinition)
        : ProvidedMethod =
        let parameters =
            [ for property in properties ->
                  let parameterType =
                      fSharpTypeToMethodParameterType classMap (property.Optional) property.FSharpType

                  if property.Optional then
                      ProvidedParameter(property.Name, parameterType, false, defaultValueForNullableType parameterType)
                  else
                      ProvidedParameter(property.Name, parameterType) ]

        ProvidedMethod(
            methodName = "Create",
            parameters = parameters,
            returnType = providedTypeDefinition,
            invokeCode = generateCreateInvokeCode schemaHashCode schemaString properties,
            isStatic = true
        )

    let private createProvidedParseMethod
        (returnType: Type)
        (schemaHashCode: int32)
        (schemaString: string)
        : ProvidedMethod =
        ProvidedMethod(
            methodName = "Parse",
            parameters = [ ProvidedParameter("json", typeof<string>) ],
            returnType = returnType,
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
                                    System.String.Join(", ", msgs) |> sprintf "JSON Schema validation failed: %s"

                            raise (System.ArgumentException(message, ((%%args[0]): string)))
                    @@>
        )

    let rec private createSubClassProvidedTypeDefinitions
        (schemaHashCode: int32)
        (schemaString: string)
        (providedTypeData: ProvidedTypeData)
        (subClasses: FSharpClassTree list)
        : Map<string, ProvidedTypeDefinition> =
        subClasses
        |> List.map (fun subClass ->
            (subClass.Name,
             fSharpClassTreeToProvidedTypeDefinition schemaHashCode schemaString providedTypeData subClass))
        |> Map.ofList

    and private fSharpClassTreeToProvidedTypeDefinition
        (schemaHashCode: int32)
        (schemaString: string)
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

        let classMap =
            createSubClassProvidedTypeDefinitions schemaHashCode schemaString providedTypeData subClasses

        classMap
        |> Map.values
        |> Seq.iter (fun subClassProvidedTypeDefinition ->
            providedTypeDefinition.AddMember(subClassProvidedTypeDefinition))

        let providedProperties = createProvidedProperties classMap properties

        providedProperties
        |> List.iter (fun providedProperty -> providedTypeDefinition.AddMember(providedProperty))

        // let createMethod =
        //     createProvidedCreateMethod classMap properties schemaHashCode schemaString providedTypeDefinition

        // providedTypeDefinition.AddMember(createMethod)

        let parseMethod =
            createProvidedParseMethod providedTypeDefinition schemaHashCode schemaString

        providedTypeDefinition.AddMember(parseMethod)

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

        fSharpClassTreeToProvidedTypeDefinition schemaHashCode (schema.ToJson()) providedTypeData fSharpClassTree
