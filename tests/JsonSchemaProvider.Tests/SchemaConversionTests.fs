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

namespace JsonSP.Tests

module SchemaConversionTests =
    open JsonSP.DesignTime.SchemaConversion
    open Expecto

    [<Literal>]
    let flatObject =
        """
        {
            "type": "object",
            "properties": {
                "X": { "type": "string" },
                "Y": { "type": "string" },
                "Z": { "type": "integer" }
            }
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

    [<Literal>]
    let nestedObjects =
        """
        {
            "type": "object",
            "properties": {
                "header": {
                    "type": "object",
                    "properties": {
                        "id": {"type": "integer"},
                        "sender": {"type": "string"},
                        "resend": {"type": "boolean"},
                        "time": {
                            "type": "object",
                            "properties": {
                                "hour": {"type": "integer"},
                                "minute": {"type": "integer"},
                                "second": {"type": "integer"}
                            },
                            "required": ["hour", "minute", "second"]
                        }
                    },
                    "required": ["id", "sender"]
                },
                "body": {
                    "type": "object",
                    "properties": {
                        "length": {"type": "integer"},
                        "payload": {"type": "string"}
                    },
                    "required": ["length", "payload"]
                }
            },
            "required": ["body"]
        }"""

    let nestedArrayWithObjectItemsShouldBeParsedCorrectly =
        test "NestedArrayWithObjectItems should be parsed correctly" {
            let actual = parseJsonSchema nestedArrayWithObjectItems

            let expected =
                JsonObject(
                    [ { Name = "values"
                        Optional = true
                        PropertyType =
                          JsonArray(
                              JsonArray(
                                  JsonObject(

                                      [ { Name = "propA"
                                          Optional = true
                                          PropertyType = JsonInteger }
                                        { Name = "propB"
                                          Optional = true
                                          PropertyType = JsonString } ]
                                  )
                              )
                          ) } ]
                )

            Expect.equal actual expected ""

        }

    let nestedObjectsShouldBeClassTreeWithFourClasses =
        test "NestedObjects should be class tree with four classes" {
            let actual =
                parseJsonSchema nestedObjects |> jsonObjectToFSharpClassTree "NestedObjects"

            let expected =
                { Name = "NestedObjects"
                  Properties =
                    [ { Name = "header"
                        Optional = true
                        FSharpType = FSharpClass("header") }
                      { Name = "body"
                        Optional = false
                        FSharpType = FSharpClass("body") } ]
                  NestedClasses =
                    [ { Name = "header"
                        Properties =
                          [ { Name = "id"
                              Optional = false
                              FSharpType = FSharpInt }
                            { Name = "sender"
                              Optional = false
                              FSharpType = FSharpString }
                            { Name = "resend"
                              Optional = true
                              FSharpType = FSharpBool }
                            { Name = "time"
                              Optional = true
                              FSharpType = FSharpClass("time") } ]
                        NestedClasses =
                          [ { Name = "time"
                              Properties =
                                [ { Name = "hour"
                                    Optional = false
                                    FSharpType = FSharpInt }
                                  { Name = "minute"
                                    Optional = false
                                    FSharpType = FSharpInt }
                                  { Name = "second"
                                    Optional = false
                                    FSharpType = FSharpInt } ]
                              NestedClasses = [] } ] }
                      { Name = "body"
                        Properties =
                          [ { Name = "length"
                              Optional = false
                              FSharpType = FSharpInt }
                            { Name = "payload"
                              Optional = false
                              FSharpType = FSharpString } ]
                        NestedClasses = [] } ] }

            Expect.equal actual expected ""
        }

    let nestedArrayWithObjectItemsShouldBeClassTreeWithTwoClasses =
        test "NestedArrayWithObjectItems should be vlass tree with two classes" {
            let actual =
                parseJsonSchema nestedArrayWithObjectItems
                |> jsonObjectToFSharpClassTree "NestedArrayWithObjectItems"

            let expected =
                { Name = "NestedArrayWithObjectItems"
                  Properties =
                    [ { Name = "values"
                        Optional = true
                        FSharpType = FSharpList(FSharpList(FSharpClass("values"))) } ]
                  NestedClasses =
                    [ { Name = "values"
                        Properties =
                          [ { Name = "propA"
                              Optional = true
                              FSharpType = FSharpInt }
                            { Name = "propB"
                              Optional = true
                              FSharpType = FSharpString } ]
                        NestedClasses = [] } ] }

            Expect.equal actual expected ""
        }

    [<Tests>]
    let tests =
        testList
            "JsonSP.Tests"
            [ nestedArrayWithObjectItemsShouldBeParsedCorrectly
              nestedArrayWithObjectItemsShouldBeClassTreeWithTwoClasses
              nestedObjectsShouldBeClassTreeWithFourClasses ]
