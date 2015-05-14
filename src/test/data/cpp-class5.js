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
                  "data": "Inner"
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
                  "gi": "fields",
                  "children": [
                    { "type": "element",
                      "gi": "field",
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
                          "data": "static"
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
                        },
                        { "type": "element",
                          "gi": "desc",
                          "children": [
                            { "type": "element",
                              "gi": "p",
                              "children": [
                                { "type": "text",
                                  "data": "The magic number \n"
                                }
                              ]
                            }
                          ]
                        }
                      ],
                      "id": "cpp/field/Foo::Inner::bar",
                      "source": "src/test/data/cpp-class5.hpp:7:16"
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
              "id": "cpp/class/Foo::Inner",
              "source": "src/test/data/cpp-class5.hpp:5:9"
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
      "source": "src/test/data/cpp-class5.hpp:2:7"
    }
  ],
  "source": "src/test/data/cpp-class5.hpp"
}
