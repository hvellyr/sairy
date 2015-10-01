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
          "gi": "constructors",
          "children": [
            { "type": "element",
              "gi": "constructor",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "Foo"
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
                  "gi": "parameters"
                },
                { "type": "element",
                  "gi": "desc",
                  "children": [
                    { "type": "element",
                      "gi": "p",
                      "children": [
                        { "type": "text",
                          "data": "ctor() inline \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/method/Foo::Foo()",
              "source": "src/test/data/cpp-class3.hpp:5:3"
            },
            { "type": "element",
              "gi": "constructor",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "Foo"
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
                  "gi": "parameters",
                  "children": [
                    { "type": "element",
                      "gi": "parameter",
                      "attributes": [
                        { "type": "text",
                          "#attr-name": "name",
                          "data": ""
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
                      ]
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
                          "data": "ctor(int) out-of-line \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/method/Foo::Foo(int)",
              "source": "src/test/data/cpp-class3.hpp:7:3"
            }
          ]
        },
        { "type": "element",
          "gi": "destructors",
          "children": [
            { "type": "element",
              "gi": "destructor",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "~Foo"
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
                  "gi": "parameters"
                },
                { "type": "element",
                  "gi": "desc",
                  "children": [
                    { "type": "element",
                      "gi": "p",
                      "children": [
                        { "type": "text",
                          "data": "dtor() out-of-line \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/method/Foo::~Foo()",
              "source": "src/test/data/cpp-class3.hpp:9:3"
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
      "source": "src/test/data/cpp-class3.hpp:2:7"
    }
  ],
  "source": "src/test/data/cpp-class3.hpp"
}
