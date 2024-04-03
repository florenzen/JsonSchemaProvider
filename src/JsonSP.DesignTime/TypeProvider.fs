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

module TypeProvider =
    open System
    open System.Reflection
    open SchemaConversion
    open TypeLevelConversion
    open ExprGenerator
    open ProviderImplementation.ProvidedTypes
    open NJsonSchema
    open JsonSchemaProvider
    open FSharp.Data

    type private ProvidedTypeData =
        { Assembly: Assembly
          NamespaceName: string
          RuntimeType: Type }

    let private createProvidedProperties
        (classMap: Map<string, ProvidedTypeDefinition>)
        (properties: FSharpProperty list)
        : ProvidedProperty list =
        [ for { Name = name
                Optional = optional
                FSharpType = fSharpType } as property in properties ->

              let plainPropertyCompileTimeType = fSharpTypeToCompileTimeType classMap fSharpType
              let plainPropertyRuntimeType = fSharpTypeToRuntimeType classMap fSharpType

              ProvidedProperty(
                  propertyName = name,
                  propertyType = optionalOrPlainType optional plainPropertyCompileTimeType,
                  getterCode = generatePropertyGetter classMap property
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

                            raise (ArgumentException(message, ((%%args[0]): string)))
                    @@>
        )

    let rec private createNestedClassProvidedTypeDefinitions
        (schemaHashCode: int32)
        (schemaString: string)
        (providedTypeData: ProvidedTypeData)
        (nestedClasses: FSharpClassTree list)
        : Map<string, ProvidedTypeDefinition> =
        nestedClasses
        |> List.map (fun nestedClass ->
            (nestedClass.Name,
             fSharpClassTreeToProvidedTypeDefinition schemaHashCode schemaString providedTypeData nestedClass true))
        |> Map.ofList

    and private fSharpClassTreeToProvidedTypeDefinition
        (schemaHashCode: int32)
        (schemaString: string)
        (providedTypeData: ProvidedTypeData)
        { Name = className
          Properties = properties
          NestedClasses = nestedClasses }
        (innerClass: bool)
        : ProvidedTypeDefinition =
        let providedTypeDefinition =
            ProvidedTypeDefinition(
                providedTypeData.Assembly,
                providedTypeData.NamespaceName,
                className + (if innerClass then "Obj" else ""),
                Some(providedTypeData.RuntimeType)
            )

        let classMap =
            createNestedClassProvidedTypeDefinitions schemaHashCode schemaString providedTypeData nestedClasses

        classMap
        |> Map.values
        |> Seq.iter (fun nestedClassProvidedTypeDefinition ->
            providedTypeDefinition.AddMember(nestedClassProvidedTypeDefinition))

        let providedProperties = createProvidedProperties classMap properties

        providedProperties
        |> List.iter (fun providedProperty -> providedTypeDefinition.AddMember(providedProperty))

        // let createMethod =
        //     createProvidedCreateMethod classMap properties schemaHashCode schemaString providedTypeDefinition

        // providedTypeDefinition.AddMember(createMethod)

        if not innerClass then
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

        fSharpClassTreeToProvidedTypeDefinition schemaHashCode (schema.ToJson()) providedTypeData fSharpClassTree false
