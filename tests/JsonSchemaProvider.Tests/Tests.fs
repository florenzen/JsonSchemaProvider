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

namespace JsonSchemaProvider.Tests

module Tests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let flatSchema =
        """{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "X": {
      "type": "string"
    },
    "Y": {
      "type": "string"
    },
    "Z": {
      "type": "integer"
    },
  }
}
"""

    [<Literal>]
    let requiredPropertiesSchema =
        """{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "X": {
      "type": "string"
    },
    "Y": {
      "type": "string"
    },
    "Z": {
      "type": "integer"
    },
  },
  "required": ["X", "Y", "Z"]
}
"""

    [<Literal>]
    let flatSchemaPath = __SOURCE_DIRECTORY__ + "/FlatSchema.json"

    [<Literal>]
    let patternSchema =
        """{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "X": {
      "type": "string",
      "pattern": "^[a-z]+$"
    }
  }
}
"""

    type Flat = JsonSchemaProvider<schema=flatSchema>
    type RequiredProperties = JsonSchemaProvider<schema=requiredPropertiesSchema>
    type FlatFromFile = JsonSchemaProvider<schemaFile=flatSchemaPath>
    type PatternSchema = JsonSchemaProvider<schema=patternSchema>

    let validRecordShouldBeParsed =
        test "valid record should be parsed" {
            let flat = Flat.Parse("""{"X": "x", "Z": 1}""")
            Expect.equal flat.X (Some("x")) """flat.X = Some("x")"""
            Expect.equal flat.Y None """flat.Y = None"""
            Expect.equal flat.Z (Some(1)) "flat.Z = Some(1)"
        }

    let createMethodShouldReturnRecord =
        test "create method should return record" {
            let flat = Flat.Create(X = "x", Z = 1)
            Expect.equal flat.X (Some("x")) """flat.X = Some("x")"""
            Expect.equal flat.Y None """flat.Y = None"""
            Expect.equal flat.Z (Some(1)) "flat.Z = Some(1)"
        }

    let requiredPropertiesShouldNotBeParsedIntoOption =
        test "required properties should not be parsed into Option" {
            let requiredProperties =
                RequiredProperties.Parse("""{"X": "x", "Y": "y", "Z": 1}""")

            Expect.equal requiredProperties.X "x" """requiredProperties.X = "x" """
            Expect.equal requiredProperties.Y "y" """requiredProperties.Y = "y" """
            Expect.equal requiredProperties.Z 1 "flat.Z = 1"
        }

    let createMethodFromFileSchemaShouldReturnRecord =
        test "create method from file schema should return record" {
            let flat = FlatFromFile.Create()
            Expect.equal flat.X None "flat.X = None"
            Expect.equal flat.Y None "flat.Y = None"
            Expect.equal flat.Z None "flat.Z = None"
        }

    let validationErrorShouldBeDetectedByCreate =
        test "validation error should be detected by Create" {
            Expect.throws (fun _ -> PatternSchema.Create(X = "a1") |> ignore) "Create throws validation exception"
        }

    let validationErrorShouldBeDetectedByParse =
        test "validation error should be detected by Parse" {
            Expect.throws
                (fun _ -> PatternSchema.Parse("""{"X": "a1"}""") |> ignore)
                "Parse throws validation exception"
        }

    [<Literal>]
    let cityPosition =
        """{
      "type": "object",
      "properties": {
        "city": {"type": "string"},
        "globalPosition": {
          "type": "object",
          "properties": {
            "lat": {"type": "number"},
            "lon": {"type": "number"}
          },
          "required": ["lat", "lon"]
        }
      },
      "required": ["city", "globalPosition"]
    }
    """

    type CityPosition = JsonSchemaProvider<schema=cityPosition>

    let valueFromNestedObjectsShouldBeCreated =
        test "value from nested objects should be created" {
            Expect.equal
                (CityPosition
                    .Create(
                        city = "Berlin",
                        globalPosition = CityPosition.globalPositionObj.Create(lat = 52.520007, lon = 13.404954)
                    )
                    .globalPosition.lat)
                50
                "create and select nested are equal"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests"
            [ validRecordShouldBeParsed
              createMethodShouldReturnRecord
              requiredPropertiesShouldNotBeParsedIntoOption
              createMethodFromFileSchemaShouldReturnRecord
              validationErrorShouldBeDetectedByCreate
              validationErrorShouldBeDetectedByParse ]
