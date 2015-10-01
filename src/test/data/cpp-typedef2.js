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
          "gi": "types",
          "children": [
            { "type": "element",
              "gi": "class",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "Dummy"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "public"
                },
                { "type": "text",
                  "#attr-name": "namespaces",
                  "data": "Foo"
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
                          "data": "Dummy type \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/class/Foo::Dummy",
              "source": "src/test/data/cpp-typedef2.hpp:7:9"
            },
            { "type": "element",
              "gi": "type-alias",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "value_type"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "public"
                },
                { "type": "text",
                  "#attr-name": "namespaces",
                  "data": "Foo"
                },
                { "type": "text",
                  "#attr-name": "referenced-type-name",
                  "data": "Foo::Xyz"
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
                          "data": "Value type \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/alias/Foo::value_type",
              "source": "src/test/data/cpp-typedef2.hpp:10:15"
            },
            { "type": "element",
              "gi": "type-alias",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "value2_type"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "public"
                },
                { "type": "text",
                  "#attr-name": "namespaces",
                  "data": "Foo"
                },
                { "type": "text",
                  "#attr-name": "referenced-type-name",
                  "data": "Foo::Dummy"
                },
                { "type": "text",
                  "#attr-name": "type-ref",
                  "data": "cpp/class/Foo::Dummy"
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
                          "data": "Value2 type \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/alias/Foo::value2_type",
              "source": "src/test/data/cpp-typedef2.hpp:12:17"
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
                  "data": "Foo's doc \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/class/Foo",
      "source": "src/test/data/cpp-typedef2.hpp:2:7"
    }
  ],
  "source": "src/test/data/cpp-typedef2.hpp"
}
