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
      "source": "src/test/data/cpp-var1.hpp:2:11"
    },
    { "type": "element",
      "gi": "variable",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "app_name"
        },
        { "type": "text",
          "#attr-name": "namespaces",
          "data": "foo::bar"
        }
      ],
      "children": [
        { "type": "element",
          "gi": "type",
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
          "gi": "desc",
          "children": [
            { "type": "element",
              "gi": "p",
              "children": [
                { "type": "text",
                  "data": "Returns the name of the app \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/var/foo::bar::app_name",
      "source": "src/test/data/cpp-var1.hpp:6:20"
    }
  ],
  "source": "src/test/data/cpp-var1.hpp"
}
