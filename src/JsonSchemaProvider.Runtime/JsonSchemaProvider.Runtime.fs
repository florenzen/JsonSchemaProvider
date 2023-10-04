namespace JsonSchemaProvider

open FSharp.Data
open FSharp.Core.CompilerServices

[<AllowNullLiteral>]
type NullableJsonValue(jsonVal: JsonValue) =
    member val JsonVal = jsonVal
    override this.ToString() : string = this.JsonVal.ToString()

#if !IS_DESIGNTIME
[<assembly:TypeProviderAssembly("JsonSchemaProvider.DesignTime")>]
do ()
#endif
