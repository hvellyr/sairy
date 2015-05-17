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
              "gi": "enum",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "Boo"
                },
                { "type": "text",
                  "#attr-name": "namespaces",
                  "data": "foo::Abc"
                }
              ],
              "children": [
                { "type": "element",
                  "gi": "enum-const",
                  "attributes": [
                    { "type": "text",
                      "#attr-name": "name",
                      "data": "k_a"
                    }
                  ],
                  "id": "cpp/enum-const/foo::Abc::Boo::k_a",
                  "source": "src/test/data/cpp-union4.hpp:6:7"
                },
                { "type": "element",
                  "gi": "enum-const",
                  "attributes": [
                    { "type": "text",
                      "#attr-name": "name",
                      "data": "k_b"
                    }
                  ],
                  "id": "cpp/enum-const/foo::Abc::Boo::k_b",
                  "source": "src/test/data/cpp-union4.hpp:7:7"
                }
              ],
              "id": "cpp/enum/foo::Abc::Boo",
              "source": "src/test/data/cpp-union4.hpp:5:10"
            },
            { "type": "element",
              "gi": "union",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "U"
                },
                { "type": "text",
                  "#attr-name": "namespaces",
                  "data": "foo::Abc"
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
                          "data": "foo::Abc::U"
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
                              "id": "cpp/field/foo::Abc::U::Bar::f",
                              "source": "src/test/data/cpp-union4.hpp:15:15"
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
                      "id": "cpp/struct/foo::Abc::U::Bar",
                      "source": "src/test/data/cpp-union4.hpp:14:14"
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
                          "data": "i"
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
                      "id": "cpp/field/foo::Abc::U::i",
                      "source": "src/test/data/cpp-union4.hpp:12:11"
                    },
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
                              "data": "foo::Abc::U::Bar"
                            },
                            { "type": "int",
                              "#attr-name": "const?",
                              "value": 0
                            }
                          ]
                        }
                      ],
                      "id": "cpp/field/foo::Abc::U::f",
                      "source": "src/test/data/cpp-union4.hpp:17:11"
                    }
                  ]
                }
              ],
              "id": "cpp/union/foo::Abc::U",
              "source": "src/test/data/cpp-union4.hpp:11:11"
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
                  "data": "boo"
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
                      "data": "foo::Abc::Boo"
                    },
                    { "type": "int",
                      "#attr-name": "const?",
                      "value": 0
                    }
                  ]
                }
              ],
              "id": "cpp/field/foo::Abc::boo",
              "source": "src/test/data/cpp-union4.hpp:9:9"
            },
            { "type": "element",
              "gi": "field",
              "attributes": [
                { "type": "text",
                  "#attr-name": "name",
                  "data": "u"
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
                      "data": "foo::Abc::U"
                    },
                    { "type": "int",
                      "#attr-name": "const?",
                      "value": 0
                    }
                  ]
                }
              ],
              "id": "cpp/field/foo::Abc::u",
              "source": "src/test/data/cpp-union4.hpp:19:7"
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
      "source": "src/test/data/cpp-union4.hpp:4:9"
    }
  ],
  "source": "src/test/data/cpp-union4.hpp"
}
