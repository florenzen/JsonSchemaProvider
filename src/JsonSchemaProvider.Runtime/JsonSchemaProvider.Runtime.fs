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

namespace JsonSchemaProvider

open System.Collections.Concurrent
open NJsonSchema
open FSharp.Data

[<AllowNullLiteral>]
type NullableJsonValue(jsonVal: JsonValue) =
    member val JsonVal = jsonVal
    override this.ToString() : string = this.JsonVal.ToString()


module SchemaCache =
    let private cache = ConcurrentDictionary<int, JsonSchema>()

    let parseSchema (schemaSource: string) =
        JsonSchema.FromJsonAsync(schemaSource)
        |> Async.AwaitTask
        |> Async.RunSynchronously

    let cacheSchema (schemaSource: string) =
        let hashCode = schemaSource.GetHashCode()
        let schema = parseSchema schemaSource
        cache[hashCode] = schema

    let retrieveSchema (hashCode: int) (schemaSource: string) =
        cache.GetOrAdd(hashCode, fun _ -> parseSchema schemaSource)
        // let (available, schema) = cache.TryGetValue(hashCode)

        // if available then
        //     schema
        // else
        //     let schema = parseSchema schemaSource
        //     cache.Add(hashCode, schema)
        //     schema

#if !IS_DESIGNTIME
[<assembly: FSharp.Core.CompilerServices.TypeProviderAssembly("JsonSchemaProvider.DesignTime")>]
do ()
#endif
