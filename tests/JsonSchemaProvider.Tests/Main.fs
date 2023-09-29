namespace JsonSchemaProvider.Tests

module Main =
    open JsonSchemaProvider.Tests
    open Expecto

    [<EntryPoint>]
    let main args =
        runTestsWithCLIArgs [] args Tests.tests
