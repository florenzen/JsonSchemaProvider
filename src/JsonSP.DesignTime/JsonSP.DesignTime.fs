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
    open JsonSchemaProvider
    open NJsonSchema

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

    let parseJsonSchema (input: string) : JsonObj =
        let schema = SchemaCache.parseSchema input

        if schema.Type <> JsonObjectType.Object then
            failwith "Only object supported"

        match parseType schema with
        | JsonObject jsonObj -> jsonObj
        | _ -> failwith "cannot happen"
    
    type FSharpClassType = {Name: string, Properties: FSharpProperty list}
    and FSharpProperty {Name: string, Optional: bool, FSharpType: FSharpType}
    and FSharpType = FSharpClass of FSharpClassType | FSharpList of FSharpType | FSharpDouble | FSharpInt | FSharpString | FSharpBool

    let toFSharpType (name: string, jsonObj: JsonObj):
        {Name: name; Properties: jsonObj.Properties |> }
