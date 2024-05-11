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

namespace JsonSchemaProvider.DesignTime

[<RequireQualifiedAccess>]
module internal CommonExprs =
    open FSharp.Quotations
    open FSharp.Quotations.Patterns
    open FSharp.Reflection
    open FSharp.Data
    open JsonSchemaProvider
    open System
    open System.Reflection

    let private cannotHappen () : 'T = failwith "Cannot happen."

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

    let private optionIsSomePropertyInfo (elementType: Type) : PropertyInfo = optionPropertyInfo elementType "IsSome"

    let private optionValuePropertyInfo (elementType: Type) : PropertyInfo = optionPropertyInfo elementType "Value"

    let private nullableHasValuePropertyInfo (elementType: Type) : PropertyInfo =
        let nullableType = (typedefof<Nullable<_>>).MakeGenericType([| elementType |])
        let properties = nullableType.GetProperties()
        let pi = properties |> Array.filter (fun pi -> pi.Name = "HasValue") |> Array.head
        pi

    let private optionSomeUnionCaseInfo (elementType: Type) : UnionCaseInfo =
        FSharpType.GetUnionCases(typedefof<_ option>.MakeGenericType(elementType))[1]

    let private optionNoneUnionCaseInfo (elementType: Type) : UnionCaseInfo =
        FSharpType.GetUnionCases(typedefof<_ option>.MakeGenericType(elementType))[0]

    let private jsonValueArrayUnionCaseInfo: UnionCaseInfo =
        FSharpType.GetUnionCases(typeof<JsonValue>)
        |> Array.filter (fun uc -> uc.Name = "Array")
        |> Array.head

    let private nullableJsonValueJsonValPropertyInfo =
        match <@@ NullableJsonValue(JsonValue.Boolean(true)).JsonVal @@> with
        | PropertyGet(_, pi, _) -> pi
        | _ -> cannotHappen ()

    let private opNotMethodInfo =
        match <@@ not true @@> with
        | Call(_, mi, _) -> mi
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

    let private arrayOfListMethodInfo (elementType: Type) : MethodInfo =
        arrayModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "OfList")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(elementType)

    let private listMapMethodInfo (fromType: Type) (toType: Type) : MethodInfo =
        listModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "Map")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(fromType, toType)

    let private listOfArryMethodInfo (elementType: Type) : MethodInfo =
        listModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "OfArray")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(elementType)

    let private opEqualityMethodInfo =
        match <@@ (=) @@> with
        | Lambda(_, Lambda(_, Call(_, mi, _))) -> mi
        | _ -> cannotHappen ()

    let getOptionIsSome (elementType: Type) (receiver: Expr) : Expr =
        let pi = optionIsSomePropertyInfo elementType
        let propertyGet = Expr.PropertyGet(pi, [ receiver ])
        propertyGet

    let getOptionValue (elementType: Type) (receiver: Expr) : Expr =
        Expr.PropertyGet(receiver, optionValuePropertyInfo elementType)

    let getNullableHasValue (elementType: Type) (receiver: Expr) : Expr =
        Expr.PropertyGet(receiver, nullableHasValuePropertyInfo elementType)

    let getNullableJsonValueJsonVal (receiver: Expr) : Expr =
        Expr.PropertyGet(receiver, nullableJsonValueJsonValPropertyInfo)

    let newOptionSome (elementType: Type) (arg: Expr) : Expr =
        Expr.NewUnionCase(optionSomeUnionCaseInfo elementType, [ arg ])

    let newOptionNone (elementType: Type) : Expr =
        Expr.NewUnionCase(optionNoneUnionCaseInfo elementType, [])

    let newJsonValueArray (arg: Expr) : Expr =
        Expr.NewUnionCase(jsonValueArrayUnionCaseInfo, [ arg ])

    let callJsonValueItem (receiver: Expr) (name: string) : Expr =
        Expr.Call(jsonValueItemMethodInfo, [ receiver; Expr.Value(name) ])

    let callJsonValueTryGetPropertyName (receiver: Expr) (name: string) : Expr =
        Expr.Call(jsonValueTryGetPropertyMethodInfo, [ receiver; Expr.Value(name) ])

    let callJsonValueAsArray (receiver: Expr) : Expr =
        Expr.Call(jsonValueAsArrayMethodInfo, [ receiver ])

    let callArrayMap (func: Expr) (array: Expr) (fromType: Type) (toType: Type) : Expr =
        Expr.Call(arrayMapMethodInfo fromType toType, [ func; array ])

    let callArrayOfList (list: Expr) (elementType: Type) : Expr =
        Expr.Call(arrayOfListMethodInfo elementType, [ list ])

    let callListOfArray (array: Expr) (elementType: Type) : Expr =
        Expr.Call(listOfArryMethodInfo elementType, [ array ])

    let callListMap (func: Expr) (list: Expr) (fromType: Type) (toType: Type) : Expr =
        Expr.Call(listMapMethodInfo fromType toType, [ func; list ])

    let callOpEquality (lhs: Expr) (rhs: Expr) : Expr =
        Expr.Call(opEqualityMethodInfo, [ lhs; rhs ])

    let callOpNot (arg: Expr) : Expr = Expr.Call(opNotMethodInfo, [ arg ])
