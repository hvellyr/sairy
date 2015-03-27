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

  std::vector<std::string> array2stringvector(const json_spirit::Array& array)
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


std::vector<Source> read_scan_db(const fs::path& file)
{
  std::vector<Source> result;

  json_spirit::Array array;
  json_spirit::Value root_elt;

  fs::ifstream inf(file);

  if (json_spirit::read_stream(inf, root_elt)) {
    // [ [ [ "projectA/includes/" ],          // include paths
    //     [ "CURRENT_PROJECT=projectA" ],    // defines
    //     [ "projectA/src/echo.cpp",
    //       "projectA/src/version.cpp" ]],   // source files
    //   ... ]

    if (root_elt.type() == json_spirit::array_type) {
      array = root_elt.get_array();

      for (const auto& v : array) {
        if (v.type() == json_spirit::array_type) {
          json_spirit::Array data = v.get_array();

          if (data.size() == 3) {
            std::vector<std::string> locincls;
            std::vector<std::string> locdefs;

            if (data[0].type() == json_spirit::array_type) {
              locincls = array2stringvector(data[0].get_array());
            }
            else {
              std::cerr << "Bad formatted scan database" << std::endl;
            }

            if (data[1].type() == json_spirit::array_type) {
              locdefs = array2stringvector(data[1].get_array());
            }
            else {
              std::cerr << "Bad formatted scan database" << std::endl;
            }

            if (data[2].type() == json_spirit::array_type) {
              json_spirit::Array locarray = data[2].get_array();

              for (const auto& loc : locarray) {
                if (loc.type() == json_spirit::str_type) {
                  result.emplace_back(fs::path(loc.get_str()), locincls,
                                      locdefs);
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
