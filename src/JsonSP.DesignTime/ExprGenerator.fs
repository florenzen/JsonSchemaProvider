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
    open FSharp.Quotations.Patterns
    open FSharp.Reflection
    open FSharp.Data
    open SchemaRep
    open TypeLevelConversions
    open JsonSchemaProvider
    open System
    open System.Reflection
    open ProviderImplementation.ProvidedTypes

    module private CommonExprs =
        let private cannotHappen (): 'T = failwith "Cannot happen."

        let private fSharpCore = typeof<List<_>>.Assembly

        let private arrayModuleType =
            fSharpCore.GetTypes() |> Array.find (fun ty -> ty.Name = "ArrayModule")

        let private listModuleType =
            fSharpCore.GetTypes() |> Array.find (fun ty -> ty.Name = "ListModule")

        let private optionPropertyInfo (elementType: Type) (propertyName: string) : PropertyInfo =
            let optionType = (typedefof<_ option>).MakeGenericType([| elementType |])
            let properties = optionType.GetProperties()
            let pi = properties |> Array.filter (fun pi -> pi.Name = propertyName) |> Array.head
            pi

        let private optionIsSomePropertyInfo (elementType: Type) : PropertyInfo =
            optionPropertyInfo elementType "IsSome"

        let private optionValuePropertyInfo (elementType: Type) : PropertyInfo = optionPropertyInfo elementType "Value"

        let private optionSomeUnionCaseInfo (elementType: Type) : UnionCaseInfo =
            FSharpType.GetUnionCases(typedefof<_ option>.MakeGenericType(elementType))[1]

        let private optionNoneUnionCaseInfo (elementType: Type) : UnionCaseInfo =
            FSharpType.GetUnionCases(typedefof<_ option>.MakeGenericType(elementType))[0]

        let private nullableJsonValueJsonValPropertyInfo =
            match <@@ NullableJsonValue(JsonValue.Boolean(true)).JsonVal @@> with
            | PropertyGet(_, pi, _) -> pi
            | _ -> cannotHappen ()

        let private jsonValueItemMethodInfo =
            match <@@ JsonValue.Record(Array.empty)["x"] @@> with
            | Call(_, mi, _) -> mi
            | _ -> cannotHappen ()

        let private jsonValueTryGetPropertyMethodInfo =
            match <@@ JsonValue.Record(Array.empty).TryGetProperty("x") @@> with
            | Call(_, mi, _) -> mi
            | _ -> cannotHappen ()

        let private jsonValueAsArrayMethodInfo =
            match <@@ JsonValue.Array(Array.empty).AsArray() @@> with
            | Call(_, mi, _) -> mi
            | _ -> cannotHappen ()

        let private arrayMapMethodInfo (fromType: Type) (toType: Type) : MethodInfo =
            arrayModuleType.GetMethods()
            |> Array.find (fun methodInfo -> methodInfo.Name = "Map")
            |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(fromType, toType)

        let private listOfArryMethodInfo (elementType: Type) : MethodInfo =
            listModuleType.GetMethods()
            |> Array.find (fun methodInfo -> methodInfo.Name = "OfArray")
            |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(elementType)

        let getOptionIsSome (elementType: Type) (receiver: Expr) : Expr =
            let pi = optionIsSomePropertyInfo elementType
            let propertyGet = Expr.PropertyGet(pi, [ receiver ])
            propertyGet

        let getOptionValue (elementType: Type) (receiver: Expr) : Expr =
            Expr.PropertyGet(receiver, optionValuePropertyInfo elementType)

        let getNullableJsonValueJsonVal (receiver: Expr) : Expr =
            Expr.PropertyGet(receiver, nullableJsonValueJsonValPropertyInfo)

        let newOptionSome (elementType: Type) (arg: Expr) : Expr =
            Expr.NewUnionCase(optionSomeUnionCaseInfo elementType, [ arg ])

        let newOptionNone (elementType: Type) : Expr =
            Expr.NewUnionCase(optionNoneUnionCaseInfo elementType, [])

        let callJsonValueItem (receiver: Expr) (name: string) : Expr =
            Expr.Call(jsonValueItemMethodInfo, [ receiver; Expr.Value(name) ])

        let callJsonValueTryGetPropertyName (receiver: Expr) (name: string) : Expr =
            Expr.Call(jsonValueTryGetPropertyMethodInfo, [ receiver; Expr.Value(name) ])

        let callJsonValueAsArray (receiver: Expr) : Expr =
            Expr.Call(jsonValueAsArrayMethodInfo, [ receiver ])

        let callArrayMap (func: Expr) (array: Expr) (fromType: Type) (toType: Type) : Expr =
            Expr.Call(arrayMapMethodInfo fromType toType, [ func; array ])

        let callListOfArray (array: Expr) (elementType: Type) : Expr =
            Expr.Call(listOfArryMethodInfo elementType, [ array ])

    let rec private generateJsonValToTypedObject
        (classMap: Map<string, ProvidedTypeDefinition>)
        (fSharpType: FSharpType)
        : Expr =
        match fSharpType with
        | FSharpBool -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsBoolean() @@>
        | FSharpClass(_) -> <@@ fun (jsonVal: JsonValue) -> NullableJsonValue(jsonVal) @@>
        | FSharpList(innerType) ->
            // <@@ fun (jsonVal: JsonValue) -> List.ofArray (Array.map %%generateForInner (jsonVal.AsArray())) @@>
            let generateForInner = generateJsonValToTypedObject classMap innerType
            let innerRuntimeType = fSharpTypeToRuntimeType classMap innerType
            let jsonValVar = Var("jsonVal", typeof<JsonValue>)
            let jsonValAsArray = CommonExprs.callJsonValueAsArray (Expr.Var(jsonValVar))

            let mappedArray =
                CommonExprs.callArrayMap generateForInner jsonValAsArray typeof<JsonValue> innerRuntimeType

            let arrayAsList = CommonExprs.callListOfArray mappedArray innerRuntimeType
            Expr.Lambda(jsonValVar, arrayAsList)
        | FSharpDouble -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsFloat() @@>
        | FSharpInt -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsInteger() @@>
        | FSharpString -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsString() @@>

    let rec private generateTypedObjectToJsonVal () : Expr = failwith "nyi"

    let generatePropertyGetter
        (classMap: Map<string, ProvidedTypeDefinition>)
        { Name = name
          Optional = optional
          FSharpType = fSharpType }
        : Expr list -> Expr =
        let plainPropertyRuntimeType = fSharpTypeToRuntimeType classMap fSharpType
        let convertToRuntimeType = generateJsonValToTypedObject classMap fSharpType

        if optional then
            fun (args: Expr list) ->
                // match %%(args[0]).JsonVal.TryGetProperty(name) with
                // | None -> None
                // | Some(jsonVal) -> Some((%%conversion) jsonVal)
                //
                // let maybeProperty = %%(args[0]).JsonVal.TryGetProperty(name)
                // if maybeProperty.IsSome then
                //     Some(%%conversion maybeProperty.Value)
                // else
                //     None
                let scrutineeVar = Var("maybeProperty", typeof<JsonValue option>)
                let jsonVal = CommonExprs.getNullableJsonValueJsonVal args[0]
                let maybePropertySelect = CommonExprs.callJsonValueTryGetPropertyName jsonVal name

                let condition =
                    CommonExprs.getOptionIsSome typeof<JsonValue> (Expr.Var(scrutineeVar))

                let thenBranch =
                    CommonExprs.newOptionSome
                        plainPropertyRuntimeType
                        (Expr.Application(
                            convertToRuntimeType,
                            CommonExprs.getOptionValue typeof<JsonValue> (Expr.Var(scrutineeVar))
                        ))

                let elseBranch = CommonExprs.newOptionNone plainPropertyRuntimeType

                Expr.Let(scrutineeVar, maybePropertySelect, Expr.IfThenElse(condition, thenBranch, elseBranch))
        else
            // %%conversion %%(args[0]).JsonVal[name]
            fun (args: Expr list) ->
                let jsonVal = CommonExprs.getNullableJsonValueJsonVal args[0]

                let propertySelect = CommonExprs.callJsonValueItem jsonVal name

                Expr.Application(convertToRuntimeType, propertySelect)

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
