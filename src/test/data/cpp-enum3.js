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
              "gi": "enum",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": ""
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
                  "gi": "enum-const",
                  "attributes": [
                    { "type": "text",
                      "#attr-name": "name",
                      "data": "k_hello"
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
                              "data": "hello\n"
                            }
                          ]
                        }
                      ]
                    }
                  ],
                  "id": "cpp/enum-const/Foo::enum.1::k_hello",
                  "source": "src/test/data/cpp-enum3.hpp:5:5"
                }
              ],
              "id": "cpp/enum/Foo::enum.1",
              "source": "src/test/data/cpp-enum3.hpp:4:3"
            },
            { "type": "element",
              "gi": "enum",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": ""
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "protected"
                },
                { "type": "text",
                  "#attr-name": "namespaces",
                  "data": "Foo"
                }
              ],
              "children": [
                { "type": "element",
                  "gi": "enum-const",
                  "attributes": [
                    { "type": "text",
                      "#attr-name": "name",
                      "data": "k_world"
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
                              "data": "world!\n"
                            }
                          ]
                        }
                      ]
                    }
                  ],
                  "id": "cpp/enum-const/Foo::enum.2::k_world",
                  "source": "src/test/data/cpp-enum3.hpp:10:5"
                }
              ],
              "id": "cpp/enum/Foo::enum.2",
              "source": "src/test/data/cpp-enum3.hpp:9:3"
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
      "source": "src/test/data/cpp-enum3.hpp:2:7"
    }
  ],
  "source": "src/test/data/cpp-enum3.hpp"
}
