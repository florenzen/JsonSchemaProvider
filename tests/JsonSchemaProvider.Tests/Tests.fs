namespace JsonSchemaProvider.Tests

module Tests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let flatSchema =
        """{
  "$id": "https://example.com/address.schema.json",
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
  "$id": "https://example.com/address.schema.json",
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

    type Flat = JsonSchemaProvider<schema=flatSchema>
    type RequiredProperties = JsonSchemaProvider<schema=requiredPropertiesSchema>

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

    let requiredPropertiesAreNotParsedIntoOption =
        test "required properties are not parsed into Option" {
            let requiredProperties = RequiredProperties.Parse("""{"X": "x", "Y": "y", "Z": 1}""")
            Expect.equal requiredProperties.X "x" """requiredProperties.X = "x" """
            Expect.equal requiredProperties.Y "y" """requiredProperties.Y = "y" """
            Expect.equal requiredProperties.Z 1 "flat.Z = 1"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests"
            [ validRecordShouldBeParsed
              createMethodShouldReturnRecord
              requiredPropertiesAreNotParsedIntoOption ]
