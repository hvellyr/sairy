{ "type": "document",
  "app-info": "cpp",
  "children": [
    { "type": "element",
      "gi": "class",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "Foo"
        },
        { "type": "text",
          "#attr-name": "namespaces",
          "data": "abc"
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
              "id": "cpp/param/abc::Foo::T",
              "source": "src/test/data/cpp11-template6.hpp:6:20"
            },
            { "type": "element",
              "gi": "templ-param",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "_"
                }
              ],
              "id": "cpp/param/abc::Foo::_",
              "source": "src/test/data/cpp11-template6.hpp:6:32"
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
                  "#attr-name": "namespaces",
                  "data": "abc"
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
              "id": "cpp/alias/abc::Type",
              "source": "src/test/data/cpp11-template6.hpp:11:9"
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
      "id": "cpp/class/abc::Foo<T, _>",
      "source": "src/test/data/cpp11-template6.hpp:7:7"
    },
    { "type": "element",
      "gi": "class",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "Foo"
        },
        { "type": "text",
          "#attr-name": "namespaces",
          "data": "abc"
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
              "id": "cpp/param/abc::Foo::T",
              "source": "src/test/data/cpp11-template6.hpp:16:20"
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
                  "data": "Foo<T, _>"
                },
                { "type": "text",
                  "#attr-name": "ref",
                  "data": "cpp/class/abc::Foo<T, _>"
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
                  "#attr-name": "namespaces",
                  "data": "abc"
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
              "id": "cpp/alias/abc::Type",
              "source": "src/test/data/cpp11-template6.hpp:21:9"
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
      "id": "cpp/class/abc::Foo<std::shared_ptr<T> >",
      "source": "src/test/data/cpp11-template6.hpp:17:7"
    }
  ],
  "source": "src/test/data/cpp11-template6.hpp"
}
