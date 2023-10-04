namespace JsonSchemaProvider

open FSharp.Data

[<AllowNullLiteral>]
type NullableJsonValue(jsonVal: JsonValue) =
    member val JsonVal = jsonVal
    override this.ToString() : string = this.JsonVal.ToString()

#if !IS_DESIGNTIME
[<assembly:FSharp.Core.CompilerServices.TypeProviderAssembly("JsonSchemaProvider.DesignTime")>]
do ()
#endif
