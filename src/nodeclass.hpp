// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once


#include <string>
#include <vector>


namespace eyestep {

enum class PropertyType { kInt, kString, kNode, kNodeList };

class Property {
public:
  Property(const std::string& nm, PropertyType ty, bool req)
      : name(nm), type(ty), isRequired(req)
  {
  }

  const std::string name;
  const PropertyType type;
  const bool isRequired;
};

using PropertySet = std::vector<Property>;

class NodeClass {
public:
  NodeClass(const std::string& cnm, const PropertySet& propSpec,
            const NodeClass* super)
      : className(cnm), propertiesSpec(propSpec), superClass(super)
  {
  }

  const std::string className;
  const PropertySet propertiesSpec;
  const NodeClass* superClass;
};


// common class definitions

const NodeClass* anyClassDefinition();
const NodeClass* rootClassDefinition();
const NodeClass* documentClassDefinition();
const NodeClass* elementClassDefinition();
const NodeClass* textClassDefinition();

const Property* findProperty(const NodeClass* nodeClass,
                             const std::string& propName);

} // ns eyestep
