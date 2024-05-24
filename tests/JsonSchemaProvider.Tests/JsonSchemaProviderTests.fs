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

namespace JsonSchemaProvider.Tests

module JsonSchemaProviderTests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let flatSchema =
        """
        {
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
        }"""

    [<Literal>]
    let requiredPropertiesSchema =
        """
        {
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
        }"""

    [<Literal>]
    let flatSchemaPath = __SOURCE_DIRECTORY__ + "/FlatSchema.json"

    [<Literal>]
    let patternSchema =
        """
        {
          "type": "object",
          "properties": {
            "X": {
              "type": "string",
              "pattern": "^[a-z]+$"
            }
          }
        }"""

    [<Literal>]
    let cityPosition =
        """
        {
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
        }"""

    [<Literal>]
    let numberArray =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {"type": "number"}
            }
          },
          "required": ["values"]
        }"""

    [<Literal>]
    let integerArray =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {"type": "integer"}
            }
          },
          "required": ["values"]
        }"""

    [<Literal>]
    let nestedArray =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {
                "type": "array",
                "items": {"type": "string"}
              }
            }
          },
          "required": ["values"]
        }"""

    [<Literal>]
    let nestedArrayWithObjectItems =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {
                "type": "array",
                "items": {
                  "type": "object",
                  "properties": {
                      "propA": {"type": "integer"},
                      "propB": {"type": "string"}
                  }
                }
              }
            }
          }
        }"""

    type Flat = JsonSchemaProvider<schema=flatSchema>
    type RequiredProperties = JsonSchemaProvider<schema=requiredPropertiesSchema>
    type FlatFromFile = JsonSchemaProvider<schemaFile=flatSchemaPath>
    type PatternSchema = JsonSchemaProvider<schema=patternSchema>
    type CityPosition = JsonSchemaProvider<schema=cityPosition>
    type NumberArray = JsonSchemaProvider<schema=numberArray>
    type IntegerArray = JsonSchemaProvider<schema=integerArray>
    type NestedArray = JsonSchemaProvider<schema=nestedArray>
    type NestedArrayWithObjectItems = JsonSchemaProvider<schema=nestedArrayWithObjectItems>

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

    let valueFromNestedObjectsShouldBeCreated =
        test "value from nested objects should be created" {
            Expect.equal
                (CityPosition
                    .Create(
                        city = "Berlin",
                        globalPosition = CityPosition.globalPositionObj.Create(lat = 52.520007, lon = 13.404954)
                    )
                    .globalPosition.lat)
                52.520007
                "create and select nested are equal"
        }

    let selectFromNumberArrayShouldYieldInputValue =
        let numArray = NumberArray.Create([ 11.0; 12.0; 11.6; 12.1 ])

        test "select from number array should yield input value" {
            Expect.equal numArray.values[1] 12.0 "numArray.values[1] = 12.0"
        }

    let selectFromIntegerArrayShouldYieldInputValue =
        let numArray = IntegerArray.Create([ 11; 12; 10; 13 ])

        test "select from integer array should yield input value" {
            Expect.equal numArray.values[1] 12 "numArray.values[1] = 12"
        }

    let selectFromNestedArrayShouldYieldInputValue =
        let array = NestedArray.Create([ [ "a"; "b" ] ])

        test "select from nested array should yield input value" {
            Expect.equal (array.values[0][1]) "b" "nestedArray.values[0][1] = \"b\""
        }

    let selectFromNestedArrayWithObjectItemsShouldYieldInputValue =
        let array =
            NestedArrayWithObjectItems.Create([ [ NestedArrayWithObjectItems.valuesObj.Create(propA = 5) ] ])

        test "select from nested array with object items should yield input value" {
            Expect.equal
                (array.values.Value[0][0]).propA
                (Some(5))
                "nestedArrayWithObjectItems.values.Value[0][0]).propA = 5"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.JsonSchemaProviderTests"
            [ validRecordShouldBeParsed
              createMethodShouldReturnRecord
              requiredPropertiesShouldNotBeParsedIntoOption
              createMethodFromFileSchemaShouldReturnRecord
              validationErrorShouldBeDetectedByCreate
              validationErrorShouldBeDetectedByParse
              valueFromNestedObjectsShouldBeCreated
              selectFromNumberArrayShouldYieldInputValue
              selectFromIntegerArrayShouldYieldInputValue
              selectFromNestedArrayShouldYieldInputValue
              selectFromNestedArrayWithObjectItemsShouldYieldInputValue ]
