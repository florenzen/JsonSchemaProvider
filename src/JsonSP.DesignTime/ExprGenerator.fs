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

module ExprGenerator =
    open FSharp.Quotations
    open FSharp.Data
    open SchemaRep
    open JsonSchemaProvider

    let rec private generateJsonValToTypedObject (fSharpType: FSharpType) : Expr =
        match fSharpType with
        | FSharpBool -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsBoolean() @@>
        | FSharpClass(_) -> <@@ fun (jsonVal: JsonValue) -> NullableJsonValue(jsonVal) @@>
        | FSharpList(innerType) ->
            let generateForInner = generateJsonValToTypedObject innerType
            <@@ fun (jsonVal: JsonValue) -> Array.map %%generateForInner (jsonVal.AsArray()) @@>
        | FSharpDouble -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsFloat() @@>
        | FSharpInt -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsInteger() @@>
        | FSharpString -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsString() @@>

    let rec private generateTypedObjectToJsonVal () : Expr = failwith "nyi"

    let generatePropertyGetter
        { Name = name
          Optional = optional
          FSharpType = fSharpType }
        : Expr list -> Expr =
        fun (args: Expr list) ->
            let conversion = generateJsonValToTypedObject fSharpType

            if optional then
                <@@
                    match ((%%args[0]): NullableJsonValue).JsonVal.TryGetProperty(name) with
                    | None -> None
                    | Some(jsonVal) -> Some((%%conversion) jsonVal)
                @@>
            else
                <@@
                    let jsonVal = ((%%args[0]): NullableJsonValue).JsonVal[name]
                    (%%conversion) jsonVal
                @@>

    let generateCreateInvokeCode
        (schemaHashCode: int32)
        (schemaSource: string)
        (properties: FSharpProperty list)
        : Expr list -> Expr =
        fun (args: Expr list) ->
            let properties: Expr =
                // [| for arg in args -> |]
                failwith "nyi"

            <@@
                let record = NullableJsonValue(JsonValue.Record(Array.concat %%properties))
                let recordSource = record.ToString()

                let schema = SchemaCache.retrieveSchema schemaHashCode schemaSource

                let validationErrors = schema.Validate(recordSource)

                if Seq.isEmpty validationErrors then
                    record
                else
                    let message =
                        validationErrors
                        |> Seq.map (fun validationError -> validationError.ToString())
                        |> fun msgs -> System.String.Join(", ", msgs) |> sprintf "JSON Schema validation failed: %s"

                    raise (System.ArgumentException(message, recordSource))
            @@>
