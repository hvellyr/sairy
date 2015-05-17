{ "type": "document",
  "app-info": "cpp",
  "children": [
    { "type": "element",
      "gi": "struct",
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
              "gi": "union",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "U"
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
                  "#attr-name": "linkage",
                  "data": "external"
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
                          "data": "i"
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
                      "id": "cpp/field/Foo::U::i",
                      "source": "src/test/data/cpp-struct4.hpp:5:9"
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
                          "data": "Inner U \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/union/Foo::U",
              "source": "src/test/data/cpp-struct4.hpp:4:9"
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
                      "data": "k_a"
                    }
                  ],
                  "id": "cpp/enum-const/Foo::enum.1::k_a",
                  "source": "src/test/data/cpp-struct4.hpp:9:5"
                },
                { "type": "element",
                  "gi": "enum-const",
                  "attributes": [
                    { "type": "text",
                      "#attr-name": "name",
                      "data": "k_b"
                    }
                  ],
                  "id": "cpp/enum-const/Foo::enum.1::k_b",
                  "source": "src/test/data/cpp-struct4.hpp:10:5"
                }
              ],
              "id": "cpp/enum/Foo::enum.1",
              "source": "src/test/data/cpp-struct4.hpp:8:3"
            }
          ]
        },
        { "type": "element",
          "gi": "fields",
          "children": [
            { "type": "element",
              "gi": "field",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "u"
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
                      "data": "Foo::U"
                    },
                    { "type": "int",
                      "#attr-name": "const?",
                      "value": 0
                    }
                  ]
                }
              ],
              "id": "cpp/field/Foo::u",
              "source": "src/test/data/cpp-struct4.hpp:13:5"
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
      "id": "cpp/struct/Foo",
      "source": "src/test/data/cpp-struct4.hpp:2:8"
    }
  ],
  "source": "src/test/data/cpp-struct4.hpp"
}
