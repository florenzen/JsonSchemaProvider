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

module SchemaRep =
    open NJsonSchema
    open JsonSchemaProvider

    type JsonProperty =
        { Name: string
          Optional: bool
          PropertyType: JsonSchemaType }

    and JsonSchemaType =
        | JsonObject of JsonProperty list
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

    let parseJsonSchemaStructured (schema: JsonSchema) : JsonSchemaType =
        if schema.Type <> JsonObjectType.Object then
            failwith "Only object supported"

        parseType schema

    let parseJsonSchema (input: string) : JsonSchemaType =
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

    type FSharpProperty =
        { Name: string
          Optional: bool
          FSharpType: FSharpType }

    type FSharpClassType =
        { Name: FSharpClassName
          Properties: FSharpProperty list }

    type FSharpRep =
        { Structure: FSharpType
          Classes: FSharpClassType list }

    type FSharpClassTree =
        { Name: string
          Properties: FSharpProperty list
          SubClasses: FSharpClassTree list }

    // let jsonPropertyToFSharpPropertyAndSubClasses
    //     (name: FSharpClassName)
    //     ({ Name = propertyName
    //        Optional = optional
    //        PropertyType = propertyType }: JsonProperty)
    //     : FSharpProperty * FSharpClassTree option =
    //     let (fSharpType, maybeClass) =
    //         match propertyType with
    //         | JsonBoolean -> (FSharpBool, None)
    //         | JsonObject(JsonObj(properties)) -> failwith "Not Implemented"
    //         | JsonArray(jsonSchemaType) ->
    //             let
    //         | JsonInteger -> (FSharpInt, None)
    //         | JsonNumber -> (FSharpDouble, None)
    //         | JsonString -> (FSharpString, None)

    //     ({ Name = propertyName
    //        Optional = optional
    //        FSharpType = fSharpType },
    //      maybeClass)

    // let jsonPropertiesToFSharpPropertiesAndSubClasses
    //     (name: FSharpClassName)
    //     (jsonSchemaProperties: JsonProperty list)
    //     : FSharpProperty list * FSharpClassTree list =
    //     // let x = jsonSchemaProperties |> List.map jsonPropertyToFSharpPropertyAndSubClasses
    //     // let classes =
    //     failwith ""

    // let jsonObjToFSharpClassTree (providedTypeName: string) (JsonObj(properties): JsonObj) : FSharpClassTree =
    //     let (properties, subClasses) =
    //         jsonPropertiesToFSharpPropertiesAndSubClasses
    //             { Name = providedTypeName
    //               Enclosing = [] }
    //             properties

    //     { Name = providedTypeName
    //       Properties = properties
    //       SubClasses = subClasses }

    let rec private jsonPropertyToFSharpProperty
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

    and private jsonSchemaTypeToFSharpRep
        ({ Name = name; Enclosing = enclosing } as fSharpClassName)
        (jsonSchemaType: JsonSchemaType)
        : FSharpRep =
        match jsonSchemaType with
        | JsonObject(properties) ->
            let fSharpPropertiesAndClasses =
                [ for property in properties ->
                      jsonPropertyToFSharpProperty
                          { Name = property.Name
                            Enclosing = enclosing @ [ name ] }
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

    let jsonSchemaTypeToFSharpRepForProvidedName
        (providedTypeName: string)
        (jsonSchemaType: JsonSchemaType)
        : FSharpRep =
        jsonSchemaTypeToFSharpRep
            { Name = providedTypeName
              Enclosing = [] }
            jsonSchemaType
