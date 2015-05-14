{ "type": "document",
  "app-info": "cpp",
  "children": [
    { "type": "element",
      "gi": "enum",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "Foo"
        }
      ],
      "children": [
        { "type": "element",
          "gi": "enum-const",
          "attributes": [
            { "type": "text",
              "#attr-name": "name",
              "data": "k_bar"
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
                      "data": "1st\n"
                    }
                  ]
                }
              ]
            }
          ],
          "id": "cpp/enum-const/Foo::k_bar",
          "source": "src/test/data/cpp-enum1.hpp:3:3"
        },
        { "type": "element",
          "gi": "enum-const",
          "attributes": [
            { "type": "text",
              "#attr-name": "name",
              "data": "k_gaz"
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
                      "data": "2nd\n"
                    }
                  ]
                }
              ]
            }
          ],
          "id": "cpp/enum-const/Foo::k_gaz",
          "source": "src/test/data/cpp-enum1.hpp:4:3"
        },
        { "type": "element",
          "gi": "enum-const",
          "attributes": [
            { "type": "text",
              "#attr-name": "name",
              "data": "k_moo"
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
                      "data": "3rd \n"
                    }
                  ]
                }
              ]
            }
          ],
          "id": "cpp/enum-const/Foo::k_moo",
          "source": "src/test/data/cpp-enum1.hpp:5:3"
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
      "id": "cpp/enum/Foo",
      "source": "src/test/data/cpp-enum1.hpp:2:6"
    },
    { "type": "element",
      "gi": "enum",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "Bar"
        }
      ],
      "children": [
        { "type": "element",
          "gi": "enum-const",
          "attributes": [
            { "type": "text",
              "#attr-name": "name",
              "data": "k_bar"
            }
          ],
          "id": "cpp/enum-const/Bar::k_bar",
          "source": "src/test/data/cpp-enum1.hpp:11:3"
        },
        { "type": "element",
          "gi": "enum-const",
          "attributes": [
            { "type": "text",
              "#attr-name": "name",
              "data": "k_gaz"
            }
          ],
          "id": "cpp/enum-const/Bar::k_gaz",
          "source": "src/test/data/cpp-enum1.hpp:12:3"
        },
        { "type": "element",
          "gi": "enum-const",
          "attributes": [
            { "type": "text",
              "#attr-name": "name",
              "data": "k_moo"
            }
          ],
          "id": "cpp/enum-const/Bar::k_moo",
          "source": "src/test/data/cpp-enum1.hpp:13:3"
        },
        { "type": "element",
          "gi": "desc",
          "children": [
            { "type": "element",
              "gi": "p",
              "children": [
                { "type": "text",
                  "data": "Bar's doc \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/enum/Bar",
      "source": "src/test/data/cpp-enum1.hpp:10:12"
    }
  ],
  "source": "src/test/data/cpp-enum1.hpp"
}
