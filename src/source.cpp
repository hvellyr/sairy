// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "source.hpp"

#include "json_spirit/json_spirit_value.h"
#include "json_spirit/json_spirit_reader_template.h"

#include <boost/filesystem/fstream.hpp>

#include <string>
#include <vector>


namespace eyestep {

namespace fs = boost::filesystem;

namespace {

  std::vector<std::string> arrayToStringVector(const json_spirit::Array& array)
  {
    std::vector<std::string> result;

    for (const auto& elt : array) {
      if (elt.type() == json_spirit::str_type) {
        result.emplace_back(elt.get_str());
      }
    }
    return result;
  }
} // anon ns


std::vector<Source> readScanDb(const fs::path& file)
{
  std::vector<Source> result;

  json_spirit::Array array;
  json_spirit::Value rootElt;

  fs::ifstream inf(file);

  if (json_spirit::read_stream(inf, rootElt)) {
    // [ [ [ "projectA/includes/" ],          // include paths
    //     [ "CURRENT_PROJECT=projectA" ],    // defines
    //     [ "projectA/src/echo.cpp",
    //       "projectA/src/version.cpp" ]],   // source files
    //   ... ]

    if (rootElt.type() == json_spirit::array_type) {
      array = rootElt.get_array();

      for (const auto& v : array) {
        if (v.type() == json_spirit::array_type) {
          json_spirit::Array data = v.get_array();

          if (data.size() == 3) {
            std::vector<std::string> locIncls;
            std::vector<std::string> locDefs;

            if (data[0].type() == json_spirit::array_type) {
              locIncls = arrayToStringVector(data[0].get_array());
            }
            else {
              std::cerr << "Bad formatted scan database" << std::endl;
            }

            if (data[1].type() == json_spirit::array_type) {
              locDefs = arrayToStringVector(data[1].get_array());
            }
            else {
              std::cerr << "Bad formatted scan database" << std::endl;
            }

            if (data[2].type() == json_spirit::array_type) {
              json_spirit::Array locArray = data[2].get_array();

              for (const auto& loc : locArray) {
                if (loc.type() == json_spirit::str_type) {
                  result.emplace_back(fs::path(loc.get_str()), locIncls,
                                      locDefs);
                }
              }
            }
            else {
              std::cerr << "Bad formatted scan database" << std::endl;
            }
          }
          else {
            std::cerr << "Bad formatted scan database" << std::endl;
          }
        }
      }
    }
  }

  return result;
}

} // ns eyestep
