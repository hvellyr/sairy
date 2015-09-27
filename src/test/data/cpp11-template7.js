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
              "source": "src/test/data/cpp11-template7.hpp:4:20"
            },
            { "type": "element",
              "gi": "templ-param",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "A"
                }
              ],
              "id": "cpp/param/Foo::A",
              "source": "src/test/data/cpp11-template7.hpp:4:32"
            },
            { "type": "element",
              "gi": "templ-param",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "B"
                }
              ],
              "id": "cpp/param/Foo::B",
              "source": "src/test/data/cpp11-template7.hpp:4:51"
            }
          ]
        },
        { "type": "element",
          "gi": "types",
          "children": [
            { "type": "element",
              "gi": "type-alias",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "Type"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "public"
                },
                { "type": "text",
                  "#attr-name": "referenced-type-name",
                  "data": "T"
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
                          "data": "The wrapped type \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/alias/Type",
              "source": "src/test/data/cpp11-template7.hpp:9:9"
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
      "id": "cpp/class/Foo<T, A, B>",
      "source": "src/test/data/cpp11-template7.hpp:5:7"
    },
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
              "source": "src/test/data/cpp11-template7.hpp:14:20"
            }
          ]
        },
        { "type": "element",
          "gi": "templ-specialization",
          "children": [
            { "type": "element",
              "gi": "specialized-from",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "Foo<T, A, B>"
                },
                { "type": "text",
                  "#attr-name": "ref",
                  "data": "cpp/class/Foo<T, A, B>"
                }
              ]
            },
            { "type": "element",
              "gi": "specialization-parameter",
              "attributes": [
                { "type": "text",
                  "#attr-name": "value",
                  "data": "shared_ptr<type-parameter-0-0>"
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
                  "data": "void"
                }
              ]
            }
          ]
        },
        { "type": "element",
          "gi": "types",
          "children": [
            { "type": "element",
              "gi": "type-alias",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "Type"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "public"
                },
                { "type": "text",
                  "#attr-name": "referenced-type-name",
                  "data": "std::shared_ptr<T>"
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
                          "data": "The wrapped type 2 \n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/alias/Type",
              "source": "src/test/data/cpp11-template7.hpp:19:9"
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
                  "data": "Foo is a specialized version for Foo \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/class/Foo<std::shared_ptr<T>, float>",
      "source": "src/test/data/cpp11-template7.hpp:15:7"
    }
  ],
  "source": "src/test/data/cpp11-template7.hpp"
}
