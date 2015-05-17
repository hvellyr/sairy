{ "type": "document",
  "app-info": "cpp",
  "children": [
    { "type": "element",
      "gi": "union",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": ""
        },
        { "type": "text",
          "#attr-name": "namespaces",
          "data": "foo"
        },
        { "type": "text",
          "#attr-name": "linkage",
          "data": "auto"
        }
      ],
      "children": [
        { "type": "element",
          "gi": "fields",
          "children": [
            { "type": "element",
              "gi": "field",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "a"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "public"
                },
                { "type": "text",
                  "#attr-name": "linkage",
                  "data": "member"
                }
              ],
              "children": [
                { "type": "element",
                  "gi": "type",
                  "attributes": [
                    { "type": "text",
                      "#attr-name": "name",
                      "data": "int"
                    },
                    { "type": "int",
                      "#attr-name": "const?",
                      "value": 0
                    }
                  ]
                }
              ],
              "id": "cpp/field/foo::union.1::a",
              "source": "src/test/data/cpp-union3.hpp:4:9"
            },
            { "type": "element",
              "gi": "field",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "b"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "public"
                },
                { "type": "text",
                  "#attr-name": "linkage",
                  "data": "member"
                }
              ],
              "children": [
                { "type": "element",
                  "gi": "type",
                  "attributes": [
                    { "type": "text",
                      "#attr-name": "name",
                      "data": "double"
                    },
                    { "type": "int",
                      "#attr-name": "const?",
                      "value": 0
                    }
                  ]
                }
              ],
              "id": "cpp/field/foo::union.1::b",
              "source": "src/test/data/cpp-union3.hpp:5:12"
            }
          ]
        },
        { "type": "element",
          "gi": "desc",
          "children": [
            { "type": "element",
              "gi": "p",
              "children": [
                { "type": "text",
                  "data": "Bar's \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/union/foo::union.1",
      "source": "src/test/data/cpp-union3.hpp:3:10"
    }
  ],
  "source": "src/test/data/cpp-union3.hpp"
}
