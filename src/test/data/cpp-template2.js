{ "type": "document",
  "app-info": "cpp",
  "children": [
    { "type": "element",
      "gi": "function",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "create_foo"
        },
        { "type": "text",
          "#attr-name": "dialect",
          "data": "cpp"
        }
      ],
      "children": [
        { "type": "element",
          "gi": "templ-params",
          "children": [
            { "type": "element",
              "gi": "templ-param",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "T"
                }
              ],
              "id": "cpp/param/create_foo::T"
            },
            { "type": "element",
              "gi": "templ-param",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "U"
                }
              ],
              "id": "cpp/param/create_foo::U"
            }
          ]
        },
        { "type": "element",
          "gi": "return-type",
          "attributes": [
            { "type": "text",
              "#attr-name": "name",
              "data": "T"
            },
            { "type": "int",
              "#attr-name": "const?",
              "value": 0
            }
          ]
        },
        { "type": "element",
          "gi": "parameters",
          "children": [
            { "type": "element",
              "gi": "parameter",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "u"
                }
              ],
              "children": [
                { "type": "element",
                  "gi": "type",
                  "attributes": [
                    { "type": "text",
                      "#attr-name": "name",
                      "data": "U"
                    },
                    { "type": "int",
                      "#attr-name": "const?",
                      "value": 0
                    }
                  ]
                }
              ]
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
                  "data": "Foo is a factory for T \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/function/create_foo(U)",
      "source": "src/test/data/cpp-template2.hpp:3:3"
    }
  ],
  "source": "src/test/data/cpp-template2.hpp"
}
