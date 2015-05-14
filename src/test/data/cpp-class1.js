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
                  "data": "get_bar"
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
                }
              ],
              "id": "cpp/method/Foo::get_bar()",
              "source": "src/test/data/cpp-class1.hpp:14:7"
            },
            { "type": "element",
              "gi": "method",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "get_bar"
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
                  "value": 1
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
                }
              ],
              "id": "cpp/method/Foo::get_bar()/const",
              "source": "src/test/data/cpp-class1.hpp:15:7"
            },
            { "type": "element",
              "gi": "method",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "set_moo"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "protected"
                },
                { "type": "text",
                  "#attr-name": "linkage",
                  "data": "static"
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
                      "data": "char *"
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
                          "data": "moo"
                        }
                      ],
                      "children": [
                        { "type": "element",
                          "gi": "type",
                          "attributes": [
                            { "type": "text",
                              "#attr-name": "name",
                              "data": "char *"
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
                }
              ],
              "id": "cpp/method/Foo::set_moo(char *)",
              "source": "src/test/data/cpp-class1.hpp:16:16"
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
      "id": "cpp/class/Foo",
      "source": "src/test/data/cpp-class1.hpp:2:7"
    }
  ],
  "source": "src/test/data/cpp-class1.hpp"
}
