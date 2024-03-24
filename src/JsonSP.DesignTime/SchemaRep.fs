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

    // type FSharpClassName =
    //     { Name: string; Enclosing: string list }

    type FSharpType =
        | FSharpClass of string
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
        { Name: string
          Properties: FSharpProperty list }

    type FSharpRep =
        { Structure: FSharpType
          Classes: FSharpClassType list }

    type FSharpClassTree =
        { Name: string
          Properties: FSharpProperty list
          SubClasses: FSharpClassTree list }

    let nestedObjects =
        { Name = "NestedObjects"
          Properties =
            [ { Name = "header"
                Optional = true
                FSharpType = FSharpClass("header") }
              { Name = "body"
                Optional = false
                FSharpType = FSharpClass("body") } ]
          SubClasses =
            [ { Name = "header"
                Properties =
                  [ { Name = "id"
                      Optional = false
                      FSharpType = FSharpInt }
                    { Name = "sender"
                      Optional = false
                      FSharpType = FSharpString }
                    { Name = "resend"
                      Optional = true
                      FSharpType = FSharpBool }
                    { Name = "time"
                      Optional = true
                      FSharpType = FSharpClass("time") } ]
                SubClasses =
                  [ { Name = "time"
                      Properties =
                        [ { Name = "hour"
                            Optional = false
                            FSharpType = FSharpInt }
                          { Name = "minute"
                            Optional = false
                            FSharpType = FSharpInt }
                          { Name = "second"
                            Optional = false
                            FSharpType = FSharpInt } ]
                      SubClasses = [] } ] }
              { Name = "body"
                Properties =
                  [ { Name = "length"
                      Optional = false
                      FSharpType = FSharpInt }
                    { Name = "payload"
                      Optional = false
                      FSharpType = FSharpString } ]
                SubClasses = [] } ] }

    let rec private jsonPropertyToFSharpPropertyAndSubClass
        ({ Name = propertyName
           Optional = optional
           PropertyType = propertyType }: JsonProperty)
        : FSharpProperty * FSharpClassTree option =
        let (fSharpType, maybeClass) =
            jsonSchemaTypeToFSharpTypeAndSubClass propertyName propertyType

        ({ Name = propertyName
           Optional = optional
           FSharpType = fSharpType },
         maybeClass)

    and private jsonSchemaTypeToFSharpTypeAndSubClass
        (lhsName: string)
        (jsonSchemaType: JsonSchemaType)
        : FSharpType * FSharpClassTree option =
        match jsonSchemaType with
        | JsonBoolean -> (FSharpBool, None)
        | JsonObject(properties) ->
            (FSharpClass(lhsName), Some(jsonPropertyListToFSharpClassTree lhsName properties))
        | JsonArray(innerType) ->
            let (innerFSharpType, maybeClass) =
                jsonSchemaTypeToFSharpTypeAndSubClass lhsName innerType

            (FSharpList(innerFSharpType), maybeClass)
        | JsonInteger -> (FSharpInt, None)
        | JsonNumber -> (FSharpDouble, None)
        | JsonString -> (FSharpString, None)
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

    and private jsonPropertyListToFSharpClassTree
        (lhsName: string)
        (jsonProperties: JsonProperty list)
        : FSharpClassTree =
        let (fSharpProperties, maybeSubClasses) =
            jsonProperties |> List.map jsonPropertyToFSharpPropertyAndSubClass |> List.unzip

        let subClasses = maybeSubClasses |> List.map Option.toList |> List.concat

        { Name = lhsName
          Properties = fSharpProperties
          SubClasses = subClasses }

    let jsonObjectToFSharpClassTree (lhsName: string) (jsonSchemaType: JsonSchemaType) : FSharpClassTree =
        match jsonSchemaType with
        | JsonObject(properties) -> jsonPropertyListToFSharpClassTree lhsName properties
        | _ -> failwith "Only object type supported"

    // let rec jsonSchemaTypeToFSharpTypeAndSubClass (fSharpClassName: FSharpClassName) (jsonSchemaType: JsonSchemaType): FSharpType * FSharpClassTree option =
    //     match jsonSchemaType with
    //     | JsonBoolean -> (FSharpBool, None)
    //     | JsonObject(properties) -> (FSharpClass(fSharpClassName), None)
    //     | JsonArray(innerType) ->
    //         let (innerFSharpType, maybeClass) = jsonSchemaTypeToFSharpTypeAndSubClass fSharpClassName innerType
    //         (FSharpList(innerFSharpType), maybeClass)
    //     | JsonInteger -> failwith "Not Implemented"
    //     | JsonNumber -> failwith "Not Implemented"
    //     | JsonString -> failwith "Not Implemented"

    let rec private jsonPropertyToFSharpProperty
        (fSharpClassName: string)
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

    and private jsonSchemaTypeToFSharpRep (fSharpClassName: string) (jsonSchemaType: JsonSchemaType) : FSharpRep =
        match jsonSchemaType with
        | JsonObject(properties) ->
            let fSharpPropertiesAndClasses =
                [ for property in properties -> jsonPropertyToFSharpProperty (property.Name) property ]

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
        jsonSchemaTypeToFSharpRep providedTypeName jsonSchemaType
