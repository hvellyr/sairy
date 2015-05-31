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
          "gi": "templ-params",
          "children": [
            { "type": "element",
              "gi": "templ-param",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "T"
                }
              ],
              "id": "cpp/param/Foo::T",
              "source": "src/test/data/cpp11-template5.hpp:2:20"
            },
            { "type": "element",
              "gi": "templ-param",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "Cont"
                }
              ],
              "id": "cpp/param/Foo::Cont",
              "source": "src/test/data/cpp11-template5.hpp:2:49"
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
                  "data": "_value"
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
                      "data": "Cont<T>"
                    },
                    { "type": "int",
                      "#attr-name": "const?",
                      "value": 0
                    },
                    { "type": "text",
                      "#attr-name": "type-ref",
                      "data": "cpp/param/Foo::Cont"
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
                          "data": "The wrapped value \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/field/_value",
              "source": "src/test/data/cpp11-template5.hpp:10:11"
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
                  "data": "Foo<T, Cont>"
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
                          "data": "t"
                        }
                      ],
                      "children": [
                        { "type": "element",
                          "gi": "type",
                          "attributes": [
                            { "type": "text",
                              "#attr-name": "name",
                              "data": "T"
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
                          "data": "Constructor \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/method/Foo<T, Cont>(T)",
              "source": "src/test/data/cpp11-template5.hpp:7:3"
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
                  "data": "Foo is a wrapper for a T \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/class/Foo<T, Cont>",
      "source": "src/test/data/cpp11-template5.hpp:3:7"
    }
  ],
  "source": "src/test/data/cpp11-template5.hpp"
}
