{ "type": "document",
  "app-info": "cpp",
  "children": [
    { "type": "element",
      "gi": "class",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "Foo"
        }
      ],
      "children": [
        { "type": "element",
          "gi": "methods",
          "children": [
            { "type": "element",
              "gi": "method",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "bar"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "public"
                },
                { "type": "text",
                  "#attr-name": "linkage",
                  "data": "member"
                },
                { "type": "int",
                  "#attr-name": "const?",
                  "value": 0
                }
              ],
              "children": [
                { "type": "element",
                  "gi": "return-type",
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
                          "data": "bar() inline \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/method/Foo::bar()",
              "source": "src/test/data/cpp-class2.hpp:5:7"
            },
            { "type": "element",
              "gi": "method",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "moo"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "protected"
                },
                { "type": "text",
                  "#attr-name": "linkage",
                  "data": "member"
                },
                { "type": "int",
                  "#attr-name": "const?",
                  "value": 0
                }
              ],
              "children": [
                { "type": "element",
                  "gi": "return-type",
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
                          "data": "Moo out-of-line \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/method/Foo::moo()",
              "source": "src/test/data/cpp-class2.hpp:8:7"
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
                  "data": "This is Foo's docu \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/class/Foo",
      "source": "src/test/data/cpp-class2.hpp:2:7"
    }
  ],
  "source": "src/test/data/cpp-class2.hpp"
}
