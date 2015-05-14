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
              "id": "cpp/field/Foo::bar",
              "source": "src/test/data/cpp-struct1.hpp:3:7"
            }
          ]
        },
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
                }
              ],
              "id": "cpp/method/Foo::Foo()",
              "source": "src/test/data/cpp-struct1.hpp:4:3"
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
                          "data": "b"
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
                }
              ],
              "id": "cpp/method/Foo::Foo(int)",
              "source": "src/test/data/cpp-struct1.hpp:6:3"
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
                }
              ],
              "id": "cpp/method/Foo::~Foo()",
              "source": "src/test/data/cpp-struct1.hpp:8:3"
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
                  "data": "get_foo"
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
                  "value": 1
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
                  "gi": "parameters"
                }
              ],
              "id": "cpp/method/Foo::get_foo()/const",
              "source": "src/test/data/cpp-struct1.hpp:10:17"
            },
            { "type": "element",
              "gi": "method",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "get_foo2"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "public"
                },
                { "type": "text",
                  "#attr-name": "linkage",
                  "data": "pure-virtual"
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
                  "gi": "parameters"
                }
              ],
              "id": "cpp/method/Foo::get_foo2()",
              "source": "src/test/data/cpp-struct1.hpp:11:17"
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
              "source": "src/test/data/cpp-struct1.hpp:14:7"
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
              "source": "src/test/data/cpp-struct1.hpp:15:7"
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
              "source": "src/test/data/cpp-struct1.hpp:16:16"
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
      "id": "cpp/struct/Foo",
      "source": "src/test/data/cpp-struct1.hpp:2:8"
    }
  ],
  "source": "src/test/data/cpp-struct1.hpp"
}
