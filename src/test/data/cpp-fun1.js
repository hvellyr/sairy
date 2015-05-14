{ "type": "document",
  "app-info": "cpp",
  "children": [
    { "type": "element",
      "gi": "namespace",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "foo"
        }
      ],
      "children": [
        { "type": "element",
          "gi": "desc",
          "children": [
            { "type": "element",
              "gi": "p",
              "children": [
                { "type": "text",
                  "data": "Foo is the toplevel namespace for all bar code \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/ns/foo",
      "source": "src/test/data/cpp-fun1.hpp:2:11"
    },
    { "type": "element",
      "gi": "function",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "boo"
        },
        { "type": "text",
          "#attr-name": "dialect",
          "data": "cpp"
        },
        { "type": "text",
          "#attr-name": "namespaces",
          "data": "foo::bar"
        }
      ],
      "children": [
        { "type": "element",
          "gi": "return-type",
          "attributes": [
            { "type": "text",
              "#attr-name": "name",
              "data": "const char *"
            },
            { "type": "int",
              "#attr-name": "const?",
              "value": 0
            }
          ]
        },
        { "type": "element",
          "gi": "parameters"
        },
        { "type": "element",
          "gi": "desc",
          "children": [
            { "type": "element",
              "gi": "p",
              "children": [
                { "type": "text",
                  "data": "A comment\n"
                }
              ]
            },
            { "type": "element",
              "gi": "p",
              "children": [
                { "type": "text",
                  "data": "with an example\n"
                }
              ]
            },
            { "type": "element",
              "gi": "example",
              "attributes": [],
              "children": [
                { "type": "element",
                  "gi": "p",
                  "children": [
                    { "type": "text",
                      "data": "let a = boo()\n"
                    }
                  ]
                }
              ]
            },
            { "type": "element",
              "gi": "admon",
              "attributes": [
                { "type": "text",
                  "#attr-name": "title",
                  "data": "Note"
                }
              ],
              "children": [
                { "type": "element",
                  "gi": "p",
                  "children": [
                    { "type": "text",
                      "data": "which is a note admonition."
                    }
                  ]
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/function/foo::bar::boo()",
      "source": "src/test/data/cpp-fun1.hpp:15:13"
    }
  ],
  "source": "src/test/data/cpp-fun1.hpp"
}
