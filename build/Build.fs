module Build

open Fake.Core
open Fake.DotNet

let initTargets () =
    Target.create "Build" (fun _ -> ())

[<EntryPoint>]
let main (args: string[]): int =
    args
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    initTargets ()
    Target.runOrDefaultWithArguments "Build"
    0
