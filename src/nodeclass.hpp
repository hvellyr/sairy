// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once


#include <string>
#include <vector>


namespace eyestep {

enum class PropertyType { k_int, k_string, k_node, k_nodelist };

class Property {
public:
  Property(const std::string& nm, PropertyType ty, bool req)
    : _name(nm), _type(ty), _is_required(req)
  {
  }

  const std::string _name;
  const PropertyType _type;
  const bool _is_required;
};

using PropertySet = std::vector<Property>;

class NodeClass {
public:
  NodeClass(const std::string& cnm, const PropertySet& prop_spec,
            const NodeClass* super)
    : _classname(cnm), _properties_spec(prop_spec), _super_class(super)
  {
  }

  const std::string _classname;
  const PropertySet _properties_spec;
  const NodeClass* _super_class;
};


// common class definitions

const NodeClass* any_class_definition();
const NodeClass* root_class_definition();
const NodeClass* document_class_definition();
const NodeClass* element_class_definition();
const NodeClass* text_class_definition();
const NodeClass* int_class_definition();

const Property* find_property(const NodeClass* node_class,
                              const std::string& propname);

} // ns eyestep
