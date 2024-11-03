# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Fixed
- Narrow dependency on `FSharp.Data` to `FSharp.Data.Json.Core`
  [#4](https://github.com/florenzen/JsonSchemaProvider/pull/4) by @xperiandri.
- Fix type provider to work in Vsual Studio
  [#5](https://github.com/florenzen/JsonSchemaProvider/pull/5) by @xperiandri.
- Update dependencies
  [#5](https://github.com/florenzen/JsonSchemaProvider/pull/5) by @xperiandri.

### Changed
- Lowered target framework to .NET Standard 2.0
  [#5](https://github.com/florenzen/JsonSchemaProvider/pull/5) by @xperiandri.

### Security
- Add reference to `System.Text.Encoding.Web` to fix vulnerability in that library
  [#5](https://github.com/florenzen/JsonSchemaProvider/pull/5) by @xperiandri.

## [0.1.0] - 2024-05-28

### Added
- A cache to avoid repeatedly parsing the schema file or string.
- A draft of the manual.
- Basic README file.
- ListTarget target for the FAKE automation.
- Meta data for the NuGet package.
- Publish package to nuget.org.
- Basic support for JSON Schemas with objects at the outer level.

### Fixed
- Nested arrays are possible.
- Fix publishing package to nuget.org.

### Changed
- The type provider uses several internal abstract representations before
  generating the code of the provided type.

## [0.1.0-pre.5] - 2024-05-25

### Added
- A cache to avoid repeatedly parsing the schema file or string.
- A draft of the manual.
- Basic README file.
- ListTarget target for the FAKE automation.
- Meta data for the NuGet package.
- Publish package to nuget.org.
- Basic support for JSON Schemas with objects at the outer level.

### Fixed
- Nested arrays are possible.
- Fix publishing package to nuget.org.

### Changed
- The type provider uses several internal abstract representations before
  generating the code of the provided type.

## [0.1.0-pre.4] - 2023-10-18

### Added
- Basic README file.
- ListTarget target for the FAKE automation.
- Meta data for the NuGet package.
- Publish package to nuget.org.
- Basic support for JSON Schemas with objects at the outer level.

### Fixed
- Fix publishing package to nuget.org.

## [0.1.0-pre.3] - 2023-10-17

### Fixed
- Fix publishing package to nuget.org.

## [0.1.0-pre.2] - 2023-10-17

### Added

[Unreleased]: https://github.com/florenzen/JsonSchemaProvider/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/florenzen/JsonSchemaProvider/releases/tag/v0.1.0
[0.1.0-pre.5]: https://github.com/florenzen/JsonSchemaProvider/releases/tag/v0.1.0-pre.5
[0.1.0-pre.4]: https://github.com/florenzen/JsonSchemaProvider/releases/tag/v0.1.0-pre.4
[0.1.0-pre.3]: https://github.com/florenzen/JsonSchemaProvider/releases/tag/v0.1.0-pre.3
[0.1.0-pre.2]: https://github.com/florenzen/JsonSchemaProvider/releases/tag/v0.1.0-pre.2
[0.1.0-pre.1]: https://github.com/florenzen/JsonSchemaProvider/releases/tag/v0.1.0-pre.1
