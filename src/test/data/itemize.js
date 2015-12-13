{ "type": "document",
  "children": [
    { "type": "element",
      "gi": "doc",
      "attributes": [],
      "children": [
        { "type": "element",
          "gi": "p",
          "children": [
            { "type": "text",
              "data": "Some text\n"
            }
          ]
        },
        { "type": "element",
          "gi": "itemize",
          "attributes": [],
          "children": [
            { "type": "element",
              "gi": "p",
              "children": [
                { "type": "element",
                  "gi": "item",
                  "attributes": []
                },
                { "type": "text",
                  "data": " first item\n"
                }
              ]
            },
            { "type": "element",
              "gi": "p",
              "children": [
                { "type": "element",
                  "gi": "item",
                  "attributes": []
                },
                { "type": "text",
                  "data": " second item which spans a row\nand contains "
                },
                { "type": "element",
                  "gi": "em",
                  "attributes": [
                    { "type": "text",
                      "#attr-name": "text",
                      "data": "some",
                      "data-attr": "text"
                    }
                  ]
                },
                { "type": "text",
                  "data": " embedded tags.\n"
                }
              ]
            },
            { "type": "element",
              "gi": "p",
              "children": [
                { "type": "element",
                  "gi": "item",
                  "attributes": [
                    { "type": "text",
                      "#attr-name": "text",
                      "data": "+",
                      "data-attr": "text"
                    }
                  ]
                },
                { "type": "text",
                  "data": " and a last, custom item\n"
                }
              ]
            }
          ]
        },
        { "type": "element",
          "gi": "p",
          "children": [
            { "type": "text",
              "data": "Some more text\n"
            }
          ]
        }
      ]
    }
  ]
}
