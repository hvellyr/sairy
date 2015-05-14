{ "type": "document",
  "app-info": "cpp",
  "children": [
    { "type": "element",
      "gi": "function",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "boo"
        },
        { "type": "text",
          "#attr-name": "dialect",
          "data": "cpp"
        }
      ],
      "children": [
        { "type": "element",
          "gi": "return-type",
          "attributes": [
            { "type": "text",
              "#attr-name": "name",
              "data": "const char *"
            },
            { "type": "int",
              "#attr-name": "const?",
              "value": 0
            }
          ]
        },
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
                  "data": "A comment\n"
                }
              ]
            },
            { "type": "element",
              "gi": "p",
              "children": [
                { "type": "text",
                  "data": "with an example\n"
                }
              ]
            },
            { "type": "element",
              "gi": "example",
              "attributes": [],
              "children": [
                { "type": "element",
                  "gi": "p",
                  "children": [
                    { "type": "text",
                      "data": "let a = boo()\n"
                    }
                  ]
                }
              ]
            },
            { "type": "element",
              "gi": "admon",
              "attributes": [
                { "type": "text",
                  "#attr-name": "title",
                  "data": "Note"
                }
              ],
              "children": [
                { "type": "element",
                  "gi": "p",
                  "children": [
                    { "type": "text",
                      "data": "which is a note admonition."
                    }
                  ]
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/function/boo()",
      "source": "src/test/data/cpp-fun2.hpp:11:13"
    },
    { "type": "element",
      "gi": "function",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "main"
        },
        { "type": "text",
          "#attr-name": "dialect",
          "data": "cpp"
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
          "gi": "parameters",
          "children": [
            { "type": "element",
              "gi": "parameter",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "argc"
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
            },
            { "type": "element",
              "gi": "parameter",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "argv"
                }
              ],
              "children": [
                { "type": "element",
                  "gi": "type",
                  "attributes": [
                    { "type": "text",
                      "#attr-name": "name",
                      "data": "char **"
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
                  "data": "Main argument\n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/function/main(int,char **)",
      "source": "src/test/data/cpp-fun2.hpp:14:5"
    }
  ],
  "source": "src/test/data/cpp-fun2.hpp"
}
