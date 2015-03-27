// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodeclass.hpp"
#include "estd/memory.hpp"

#include <algorithm>
#include <string>
#include <vector>


namespace eyestep {

const NodeClass* any_class_definition()
{
  static NodeClass any_class{"any", PropertySet(), nullptr};
  return &any_class;
}

const NodeClass* root_class_definition()
{
  static NodeClass root_class{"root",
                              PropertySet{{"start-time", PropertyType::k_string,
                                           false},
                                          {"end-time", PropertyType::k_string,
                                           false}},
                              any_class_definition()};
  return &root_class;
}

const NodeClass* document_class_definition()
{
  static NodeClass doc_class{"document",
                             PropertySet{
                               {"source", PropertyType::k_string, false},
                               {"app-info", PropertyType::k_string, false},
                               {"children", PropertyType::k_nodelist,
                                true}, // conprop=content
                             },
                             any_class_definition()};
  return &doc_class;
}


const NodeClass* element_class_definition()
{
  static NodeClass elt_class{"element",
                             PropertySet{
                               {"source", PropertyType::k_string, false},
                               {"children", PropertyType::k_nodelist,
                                true}, // conprop=content
                               {"gi", PropertyType::k_string, true},
                               {"id", PropertyType::k_string, false},
                             },
                             any_class_definition()};
  return &elt_class;
}


const NodeClass* text_class_definition()
{
  static NodeClass elt_class{"text",
                             PropertySet{
                               {"data", PropertyType::k_string, false},
                             },
                             any_class_definition()};
  return &elt_class;
}


const Property* find_property(const NodeClass* node_class,
                              const std::string& propname)
{
  const NodeClass* p = node_class;
  while (p) {
    auto i_prop =
      std::find_if(p->_properties_spec.begin(), p->_properties_spec.end(),
                   [&propname](const Property& prop) {
                     return prop._name == propname;
                   });
    if (i_prop != p->_properties_spec.end()) {
      return &(*i_prop);
    }

    p = p->_super_class;
  }

  return nullptr;
}

} // ns eyestep
