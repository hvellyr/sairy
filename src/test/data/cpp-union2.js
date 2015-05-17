{ "type": "document",
  "app-info": "cpp",
  "children": [
    { "type": "element",
      "gi": "union",
      "attributes": [
        { "type": "text",
          "#attr-name": "name",
          "data": "Abc"
        },
        { "type": "text",
          "#attr-name": "namespaces",
          "data": "foo"
        },
        { "type": "text",
          "#attr-name": "linkage",
          "data": "external"
        }
      ],
      "children": [
        { "type": "element",
          "gi": "types",
          "children": [
            { "type": "element",
              "gi": "struct",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "Bar"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "public"
                },
                { "type": "text",
                  "#attr-name": "namespaces",
                  "data": "foo::Abc"
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
                          "data": "f"
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
                              "data": "float"
                            },
                            { "type": "int",
                              "#attr-name": "const?",
                              "value": 0
                            }
                          ]
                        }
                      ],
                      "id": "cpp/field/foo::Abc::Bar::f",
                      "source": "src/test/data/cpp-union2.hpp:7:13"
                    },
                    { "type": "element",
                      "gi": "field",
                      "attributes": [
                        { "type": "text",
                          "#attr-name": "name",
                          "data": "name"
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
                              "data": "const char *"
                            },
                            { "type": "int",
                              "#attr-name": "const?",
                              "value": 0
                            }
                          ]
                        }
                      ],
                      "id": "cpp/field/foo::Abc::Bar::name",
                      "source": "src/test/data/cpp-union2.hpp:8:19"
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
                          "data": "the bar handle\n"
                        }
                      ]
                    }
                  ]
                }
              ],
              "id": "cpp/struct/foo::Abc::Bar",
              "source": "src/test/data/cpp-union2.hpp:6:12"
            },
            { "type": "element",
              "gi": "class",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "Mno"
                },
                { "type": "text",
                  "#attr-name": "access",
                  "data": "public"
                },
                { "type": "text",
                  "#attr-name": "namespaces",
                  "data": "foo::Abc"
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
                          "data": "ixwick"
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
                        },
                        { "type": "element",
                          "gi": "desc",
                          "children": [
                            { "type": "element",
                              "gi": "p",
                              "children": [
                                { "type": "text",
                                  "data": "unknown\n"
                                }
                              ]
                            }
                          ]
                        }
                      ],
                      "id": "cpp/field/foo::Abc::Mno::ixwick",
                      "source": "src/test/data/cpp-union2.hpp:14:11"
                    },
                    { "type": "element",
                      "gi": "field",
                      "attributes": [
                        { "type": "text",
                          "#attr-name": "name",
                          "data": "raa"
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
                              "data": "double"
                            },
                            { "type": "int",
                              "#attr-name": "const?",
                              "value": 0
                            }
                          ]
                        }
                      ],
                      "id": "cpp/field/foo::Abc::Mno::raa",
                      "source": "src/test/data/cpp-union2.hpp:15:14"
                    }
                  ]
                }
              ],
              "id": "cpp/class/foo::Abc::Mno",
              "source": "src/test/data/cpp-union2.hpp:12:11"
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
                      "data": "foo::Abc::Bar"
                    },
                    { "type": "int",
                      "#attr-name": "const?",
                      "value": 0
                    }
                  ]
                }
              ],
              "id": "cpp/field/foo::Abc::bar",
              "source": "src/test/data/cpp-union2.hpp:10:9"
            },
            { "type": "element",
              "gi": "field",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "mno"
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
                      "data": "class Mno"
                    },
                    { "type": "int",
                      "#attr-name": "const?",
                      "value": 0
                    }
                  ]
                }
              ],
              "id": "cpp/field/foo::Abc::mno",
              "source": "src/test/data/cpp-union2.hpp:16:7"
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
                  "data": "A union \n"
                }
              ]
            }
          ]
        }
      ],
      "id": "cpp/union/foo::Abc",
      "source": "src/test/data/cpp-union2.hpp:4:9"
    }
  ],
  "source": "src/test/data/cpp-union2.hpp"
}
