// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodeclass.hpp"
#include "estd/memory.hpp"

#include <algorithm>
#include <string>
#include <vector>


namespace eyestep {

const NodeClass* anyClassDefinition()
{
  static NodeClass anyClass{"any", PropertySet(), nullptr};
  return &anyClass;
}

const NodeClass* rootClassDefinition()
{
  static NodeClass rootClass{"root",
                             PropertySet{{"start-time", PropertyType::kString,
                                          false},
                                         {"end-time", PropertyType::kString,
                                          false}},
                             anyClassDefinition()};
  return &rootClass;
}

const NodeClass* documentClassDefinition()
{
  static NodeClass docClass{"document",
                            PropertySet{
                              {"source", PropertyType::kString, false},
                              {"app-info", PropertyType::kString, false},
                              {"children", PropertyType::kNodeList,
                               true}, // conprop=content
                            },
                            anyClassDefinition()};
  return &docClass;
}


const NodeClass* elementClassDefinition()
{
  static NodeClass eltClass{"element",
                            PropertySet{
                              {"source", PropertyType::kString, false},
                              {"children", PropertyType::kNodeList,
                               true}, // conprop=content
                              {"gi", PropertyType::kString, true},
                              {"id", PropertyType::kString, false},
                            },
                            anyClassDefinition()};
  return &eltClass;
}


const NodeClass* textClassDefinition()
{
  static NodeClass eltClass{"text",
                            PropertySet{
                              {"data", PropertyType::kString, false},
                            },
                            anyClassDefinition()};
  return &eltClass;
}


const Property* findProperty(const NodeClass* nodeClass,
                             const std::string& propName)
{
  const NodeClass* p = nodeClass;
  while (p) {
    auto i_prop =
      std::find_if(p->propertiesSpec.begin(), p->propertiesSpec.end(),
                   [&propName](const Property& prop) {
                     return prop.name == propName;
                   });
    if (i_prop != p->propertiesSpec.end()) {
      return &(*i_prop);
    }

    p = p->superClass;
  }

  return nullptr;
}

} // ns eyestep
