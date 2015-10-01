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
              "gi": "struct",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "Xyz"
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
                  "gi": "types",
                  "children": [
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
                          "data": "Foo::Xyz"
                        },
                        { "type": "text",
                          "#attr-name": "referenced-type-name",
                          "data": "int"
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
                                  "data": "The value \n"
                                }
                              ]
                            }
                          ]
                        }
                      ],
                      "id": "cpp/alias/Foo::Xyz::value_type",
                      "source": "src/test/data/cpp-using3.hpp:7:11"
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
                          "data": "public inner \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/struct/Foo::Xyz",
              "source": "src/test/data/cpp-using3.hpp:5:10"
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
                  "data": "Xyz::value_type"
                },
                { "type": "text",
                  "#attr-name": "type-ref",
                  "data": "cpp/alias/Foo::Xyz::value_type"
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
              "source": "src/test/data/cpp-using3.hpp:11:9"
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
      "source": "src/test/data/cpp-using3.hpp:2:7"
    }
  ],
  "source": "src/test/data/cpp-using3.hpp"
}
