{ "type": "document",
  "app-info": "cpp",
  "children": [
    { "type": "element",
      "gi": "struct",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "Foo"
        },
        { "type": "text",
          "#attr-name": "namespaces",
          "data": "maa"
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
                  "data": "virtual"
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
                      "data": "void"
                    },
                    { "type": "int",
                      "#attr-name": "const?",
                      "value": 0
                    }
                  ]
                },
                { "type": "element",
                  "gi": "parameters"
                }
              ],
              "id": "cpp/method/maa::Foo::bar()",
              "source": "src/test/data/cpp-struct3.hpp:4:16"
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
                  "data": "A base struct \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/struct/maa::Foo",
      "source": "src/test/data/cpp-struct3.hpp:3:8"
    },
    { "type": "element",
      "gi": "struct",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "Foo2"
        }
      ],
      "children": [
        { "type": "element",
          "gi": "inherits",
          "children": [
            { "type": "element",
              "gi": "inherit",
              "attributes": [
                { "type": "text",
                  "#attr-name": "base-ref",
                  "data": "cpp/struct/maa::Foo"
                },
                { "type": "text",
                  "#attr-name": "name",
                  "data": "maa::Foo"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "public"
                },
                { "type": "int",
                  "#attr-name": "virtual?",
                  "value": 0
                }
              ]
            }
          ]
        },
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
                  "data": "virtual"
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
                      "data": "void"
                    },
                    { "type": "int",
                      "#attr-name": "const?",
                      "value": 0
                    }
                  ]
                },
                { "type": "element",
                  "gi": "parameters"
                }
              ],
              "id": "cpp/method/Foo2::bar()",
              "source": "src/test/data/cpp-struct3.hpp:12:8"
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
                  "data": "A struct \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/struct/Foo2",
      "source": "src/test/data/cpp-struct3.hpp:11:8"
    }
  ],
  "source": "src/test/data/cpp-struct3.hpp"
}
