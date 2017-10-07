// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodeclass.hpp"
#include "estd/memory.hpp"
#include "nodes.hpp"

#include <algorithm>
#include <string>
#include <vector>


namespace eyestep {

const NodeClass* any_class_definition() {
  static auto any_class =
    NodeClass{"any",
              PropertySet{{CommonProps::k_attr_name, PropertyType::k_string, false},
                          {CommonProps::k_auto_id, PropertyType::k_string, false}},
              nullptr};
  return &any_class;
}

const NodeClass* root_class_definition() {
  static auto root_class =
    NodeClass{"root", PropertySet{{"start-time", PropertyType::k_string, false},
                                  {"end-time", PropertyType::k_string, false}},
              any_class_definition()};
  return &root_class;
}

const NodeClass* document_class_definition() {
  static auto doc_class =
    NodeClass{"document",
              PropertySet{
                {CommonProps::k_source, PropertyType::k_string, false},
                {"app-info", PropertyType::k_string, false},
                {CommonProps::k_children, PropertyType::k_nodelist,
                 true}, // conprop=content
              },
              any_class_definition()};
  return &doc_class;
}


const NodeClass* element_class_definition() {
  static auto elt_class =
    NodeClass{"element",
              PropertySet{
                {CommonProps::k_source, PropertyType::k_string, false},
                {CommonProps::k_children, PropertyType::k_nodelist,
                 true}, // conprop=content
                {CommonProps::k_gi, PropertyType::k_string, true},
                {CommonProps::k_id, PropertyType::k_string, false},
              },
              any_class_definition()};
  return &elt_class;
}


const NodeClass* text_class_definition() {
  static auto elt_class =
    NodeClass{"text",
              PropertySet{
                {CommonProps::k_data, PropertyType::k_string, false},
              },
              any_class_definition()};
  return &elt_class;
}


const NodeClass* int_class_definition() {
  static auto elt_class = NodeClass{"int",
                                    PropertySet{
                                      {CommonProps::k_value, PropertyType::k_int, false},
                                    },
                                    any_class_definition()};
  return &elt_class;
}


const Property* find_property(const NodeClass* node_class, const std::string& propname) {
  using namespace std;

  auto* p = node_class;
  while (p) {
    auto i_prop =
      find_if(begin(p->_properties_spec), end(p->_properties_spec),
              [&propname](const Property& prop) { return prop._name == propname; });
    if (i_prop != p->_properties_spec.end())
      return &(*i_prop);

    p = p->_super_class;
  }

  return nullptr;
}

} // ns eyestep
