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


open System
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

module SchemaRep =
    type JsonProperty =
        { Name: string
          Optional: bool
          PropertyType: JsonSchemaType }

    and JsonObj = JsonObj of JsonProperty list

    and JsonSchemaType =
        | JsonObject of JsonObj
        | JsonArray of JsonSchemaType
        | JsonBoolean
        | JsonInteger
        | JsonNumber
        | JsonString

    let rec private parseObject (schema: JsonSchema) : JsonSchemaType =
        let isOptional (name: string) =
            not (schema.RequiredProperties.Contains(name))

        schema.Properties
        |> Seq.map (fun pair ->
            { Name = pair.Key
              Optional = isOptional (pair.Key)
              PropertyType = parseType pair.Value })
        |> Seq.toList
        |> JsonObj
        |> JsonObject

    and private parseArray (schema: JsonSchema) : JsonSchemaType = parseType schema.Item |> JsonArray

    and private parseType (schema: JsonSchema) : JsonSchemaType =
        match schema.Type with
        | JsonObjectType.Array -> parseArray schema
        | JsonObjectType.Boolean -> JsonBoolean
        | JsonObjectType.Integer -> JsonInteger
        | JsonObjectType.Number -> JsonNumber
        | JsonObjectType.Object -> parseObject schema
        | JsonObjectType.String -> JsonString
        | _ -> failwithf "Unsupported JSON object type %O" schema.Type

    let parseJsonSchemaStructured (schema: JsonSchema) : JsonObj =
        if schema.Type <> JsonObjectType.Object then
            failwith "Only object supported"

        match parseType schema with
        | JsonObject jsonObj -> jsonObj
        | _ -> failwith "cannot happen"

    let parseJsonSchema (input: string) : JsonObj =
        let schema = SchemaCache.parseSchema input
        parseJsonSchemaStructured schema

    type FSharpClassName =
        { Name: string; Enclosing: string list }

    type FSharpType =
        | FSharpClass of FSharpClassName
        | FSharpList of FSharpType
        | FSharpDouble
        | FSharpInt
        | FSharpString
        | FSharpBool

    type FSharpClassType =
        { Name: FSharpClassName
          Properties: FSharpProperty list }

    and FSharpProperty =
        { Name: string
          Optional: bool
          FSharpType: FSharpType }

    type FSharpRep =
        { Structure: FSharpType
          Classes: FSharpClassType list }

    let rec jsonPropertyToFSharpProperty
        (fSharpClassName: FSharpClassName)
        (jsonProperty: JsonProperty)
        : FSharpProperty * FSharpClassType list =
        let { Structure = fSharpType
              Classes = classes } =
            jsonSchemaTypeToFSharpRep fSharpClassName (jsonProperty.PropertyType)

        let fSharpProperty =
            { Name = jsonProperty.Name
              Optional = jsonProperty.Optional
              FSharpType = fSharpType }

        (fSharpProperty, classes)

    and jsonSchemaTypeToFSharpRep
        ({ Name = name; Enclosing = enclosing } as fSharpClassName)
        (jsonSchemType: JsonSchemaType)
        : FSharpRep =
        match jsonSchemType with
        | JsonObject(JsonObj(properties)) ->
            let fSharpPropertiesAndClasses =
                [ for property in properties ->
                      jsonPropertyToFSharpProperty
                          { Name = property.Name
                            Enclosing = name :: enclosing }
                          property ]

            let (properties, innerClasses) = List.unzip fSharpPropertiesAndClasses

            let classFromProperties =
                { Name = fSharpClassName
                  Properties = properties }


            { Structure = FSharpClass(fSharpClassName)
              Classes = classFromProperties :: List.concat innerClasses }
        | JsonArray(jsonSchemaType) ->
            let { Structure = innerStructure
                  Classes = innerClasses } =
                jsonSchemaTypeToFSharpRep fSharpClassName jsonSchemaType

            { Structure = FSharpList(innerStructure)
              Classes = innerClasses }
        | JsonBoolean -> { Structure = FSharpBool; Classes = [] }
        | JsonInteger -> { Structure = FSharpInt; Classes = [] }
        | JsonNumber ->
            { Structure = FSharpDouble
              Classes = [] }
        | JsonString ->
            { Structure = FSharpString
              Classes = [] }

    let jsonObjToFSharpRep (providedTypeName: string) (jsonObj: JsonObj) : FSharpRep =
        jsonSchemaTypeToFSharpRep
            { Name = providedTypeName
              Enclosing = [] }
            (JsonObject(jsonObj))
// let rec private jsonToFSharpProperty
//     (enclosing: string option)
//     ({ Name = name
//        Optional = optional
//        PropertyType = propertyType }: JsonProperty)
//     : FSharpProperty * FSharpClassType list =
//     let (fsType, classes) = jsonToFSharpType name enclosing propertyType

//     ({ Name = name
//        Optional = optional
//        FSharpType = fsType },
//      classes)

// and private jsonObjToFSharpType (name: string) (JsonObj(jsonProperties): JsonObj) : FSharpClassType list =
//     let fsPropertiesAndClasses =
//         jsonProperties |> List.map (jsonToFSharpProperty (Some(name)))

//     let (fsProperties, classes) = List.unzip fsPropertiesAndClasses

//     { Name = name
//       Properties = fsProperties }
//     :: List.concat classes

// and private jsonToFSharpType
//     (enclosing: string list)
//     (jsonType: JsonSchemaType)
//     : FSharpType * FSharpClassType list =
//     match jsonType with
//     | JsonObject(jsonObj) -> (FSharpClass({Name:name, enclosing), (jsonObjToFSharpType name jsonObj))
//     | JsonArray(elemType) ->
//         let (fsType, classes) = jsonToFSharpType name enclosing elemType
//         (FSharpList(fsType), classes)
//     | JsonBoolean -> (FSharpBool, [])
//     | JsonInteger -> (FSharpInt, [])
//     | JsonNumber -> (FSharpDouble, [])
//     | JsonString -> (FSharpString, [])

// let transformJsonObjToFSharpType (name: string) (jsonObj: JsonObj) = jsonObjToFSharpType name jsonObj

module TypeProvider =
    open SchemaRep

    type ProvidedTypeData =
        { Assembly: Assembly
          NamespaceName: string
          RuntimeType: Type }


    // let rec fSharpToDotNetType (name: (fSharpType: FSharpType): Type=
    //     match fSharpType with
    //     | JsonObject(jsonObj)
    //     | JsonString -> typeof<string>

    let run schema (assembly: Assembly) (namespaceName: string) (typeName: string) (runtimeType: Type) =
        // let providedTypeData = {Assembly: assembly; NamespaceName=namespaceName;RuntimeType=runtimeType}
        // let jsonObj = parseJsonSchemaStructured schema
        // let fsharpType = transformJsonObjToFSharpType jsonObj
        failwith ""


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

    let jsonSchemaTy =
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

            if schema.Type <> JsonObjectType.Object then
                failwith "Only object supported"

            let providedType =
                TypeProvider.run schema thisAssembly namespaceName typeName runtimeType

            providedType
        | paramValues -> failwithf "Unexpected parameter values %O" paramValues

    do
        jsonSchemaTy.DefineStaticParameters(parameters = staticParams, instantiationFunction = instantiate)

        this.AddNamespace(namespaceName, [ jsonSchemaTy ])
