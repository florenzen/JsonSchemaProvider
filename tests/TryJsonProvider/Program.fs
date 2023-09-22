// For more information see https://aka.ms/fsharp-console-apps
open JsonSchemaProvider

printfn "Hello from F#"

type FooPair = Pair<prefix="Fob">

let foo = FooPair("1", "2")

printfn "%s" (foo.FobFst())

printfn "%s" (foo.FobSnd())

printfn "%s" (foo.FobSnd())

let x = FooPair.Baz()

printfn "%s" (x.FobSnd())
