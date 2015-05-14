{ "type": "document",
  "app-info": "cpp",
  "children": [
    { "type": "element",
      "gi": "class",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "Dummy"
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
                  "data": "Dummy type \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/class/Dummy",
      "source": "src/test/data/cpp-typedef1.hpp:6:7"
    },
    { "type": "element",
      "gi": "type-alias",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "Foo"
        },
        { "type": "text",
          "#attr-name": "namespaces",
          "data": "moo"
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
                  "data": "A number \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/alias/moo::Foo",
      "source": "src/test/data/cpp-typedef1.hpp:11:13"
    },
    { "type": "element",
      "gi": "type-alias",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "Bar"
        },
        { "type": "text",
          "#attr-name": "namespaces",
          "data": "moo"
        },
        { "type": "text",
          "#attr-name": "referenced-type-name",
          "data": "std::vector<int>"
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
                  "data": "A container \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/alias/moo::Bar",
      "source": "src/test/data/cpp-typedef1.hpp:14:26"
    },
    { "type": "element",
      "gi": "type-alias",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "MyXyz"
        },
        { "type": "text",
          "#attr-name": "namespaces",
          "data": "moo"
        },
        { "type": "text",
          "#attr-name": "referenced-type-name",
          "data": "Xyz"
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
                  "data": "My type \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/alias/moo::MyXyz",
      "source": "src/test/data/cpp-typedef1.hpp:17:13"
    },
    { "type": "element",
      "gi": "type-alias",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "MyDummy"
        },
        { "type": "text",
          "#attr-name": "namespaces",
          "data": "moo"
        },
        { "type": "text",
          "#attr-name": "referenced-type-name",
          "data": "Dummy"
        },
        { "type": "text",
          "#attr-name": "type-ref",
          "data": "cpp/class/Dummy"
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
                  "data": "Another type \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/alias/moo::MyDummy",
      "source": "src/test/data/cpp-typedef1.hpp:20:15"
    }
  ],
  "source": "src/test/data/cpp-typedef1.hpp"
}
