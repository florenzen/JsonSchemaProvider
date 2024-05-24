# JsonSchemaProvider: F# type provider for JSON schema

[![NuGet Badge](https://buildstats.info/nuget/JsonSchemaProvider?includePreReleases=true)](https://www.nuget.org/packages/JsonSchemaProvider/)
[![GitHub Actions](https://github.com/florenzen/JsonSchemaProvider/actions/workflows/main.yml/badge.svg?branch=main)](https://github.com/florenzen/JsonSchemaProvider/actions/workflows/main.yml?branch=main)

The JsonSchemaProvider provides F# types from [JSON schemas](https://json-schema.org). It can be used to build
JSON values in a strongly typed way that conform to the schema or to parse JSON values into an F# value that
can be queried in a stringly types way. Specifications like numeric ranges or string patterns that cannot be
validated at compile time are checked at runtime.

The JSON schema can either be given as an inline string literal or by a local file.

The type provider is built around [NJsonSchema](https://njsonschema.org/) for the schema parsing and validation
and uses the `JsonValue` data type from [FSharp.Data](https://fsprojects.github.io/FSharp.Data/).

The version history is kept in the [changelog](CHANGELOG.md).

See the [documentation](https://florenzen.github.io/JsonSchemaProvider) for instructions and
examples how to use the type provider.

## Building

The type provider requires the .NET SDK 7 or higher.

The code comes with a [Dev Container specification](.devcontainer/devcontainer.json) that sets up the necessary
tools and the .NET SDK.

When not inside the Dev Container, issue a

```bash
dotnet tool restore
```

to install the .NET tools listed in [dotnet-tools.json](.config/dotnet-tools.json).

The code is built using [FAKE](https://fake.build) as follows.

On Linux/macOS:

```bash
./build.sh
```

On Windows:

```powershell
build.cmd
```

The FAKE build script is based on [MiniScaffold](https://github.com/TheAngryByrd/MiniScaffold) and provides
most of its build targets. The list of available targets can be obtained by

```bash
./build.sh ListTargets
```

### Environment Variables

- `CONFIGURATION` will set the [configuration](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x#options) of the dotnet commands. 
  If not set, it will default to Release.
  - `CONFIGURATION=Debug ./build.sh` will result in `-c` additions to commands such as in `dotnet build -c Debug`
- `ENABLE_COVERAGE` Will enable running code coverage metrics.  AltCover can have
  [severe performance degradation](https://github.com/SteveGilham/altcover/issues/57) so code coverage
  evaluation are disabled by default to speed up the feedback loop.
  - `ENABLE_COVERAGE=1 ./build.sh` will enable code coverage evaluation

## Debugging

Debugging type providers requires to run the FSharp compiler or interpreter on a source
file using the type provider since the provider's code is executed in the compilation
pipeline. See the comments in [debugUtils/debug.fsx](debugUtils/debug.fsx) how to launch the
code in the Ionide debugger.

## License

The JSON schema type provider is available under the MIT license. For more information see [license file](LICENSE).
