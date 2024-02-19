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

module SchemaRepTests =
    open JsonSP.DesignTime.SchemaRep
    open Expecto

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

    let test1 =
        test "test1" {
            let actual = parseJsonSchema nestedArrayWithObjectItems

            let expected =
                JsonObj(
                    [ { Name = "values"
                        Optional = true
                        PropertyType =
                          JsonArray(
                              JsonArray(
                                  JsonObject(
                                      JsonObj
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

    [<Tests>]
    let tests = testList "JsonSP.Tests" [ test1 ]
