{ "type": "document",
  "app-info": "cpp",
  "children": [
    { "type": "element",
      "gi": "function",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "foo"
        },
        { "type": "text",
          "#attr-name": "dialect",
          "data": "cpp"
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
              "id": "cpp/param/foo::T"
            },
            { "type": "element",
              "gi": "templ-param",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "kInt"
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
              "id": "cpp/param/foo::kInt"
            },
            { "type": "element",
              "gi": "templ-param",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "kBool"
                }
              ],
              "children": [
                { "type": "element",
                  "gi": "type",
                  "attributes": [
                    { "type": "text",
                      "#attr-name": "name",
                      "data": "bool"
                    },
                    { "type": "int",
                      "#attr-name": "const?",
                      "value": 0
                    }
                  ]
                }
              ],
              "id": "cpp/param/foo::kBool"
            }
          ]
        },
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
      "id": "cpp/function/foo()",
      "source": "src/test/data/cpp11-template8.hpp:2:6"
    },
    { "type": "element",
      "gi": "function",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "foo"
        },
        { "type": "text",
          "#attr-name": "dialect",
          "data": "cpp"
        }
      ],
      "children": [
        { "type": "element",
          "gi": "templ-specialization",
          "children": [
            { "type": "element",
              "gi": "specialized-from",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "foo()"
                },
                { "type": "text",
                  "#attr-name": "ref",
                  "data": "cpp/function/foo()"
                }
              ]
            },
            { "type": "element",
              "gi": "specialization-parameter",
              "attributes": [
                { "type": "text",
                  "#attr-name": "value",
                  "data": "float"
                }
              ]
            },
            { "type": "element",
              "gi": "specialization-parameter",
              "attributes": [
                { "type": "text",
                  "#attr-name": "value",
                  "data": "-7"
                }
              ]
            },
            { "type": "element",
              "gi": "specialization-parameter",
              "attributes": [
                { "type": "text",
                  "#attr-name": "value",
                  "data": "1"
                }
              ]
            }
          ]
        },
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
      "id": "cpp/function/foo()",
      "source": "src/test/data/cpp11-template8.hpp:6:6"
    }
  ],
  "source": "src/test/data/cpp11-template8.hpp"
}
