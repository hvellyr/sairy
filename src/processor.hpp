// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fo.hpp"

#include "cxxopts.hpp"
#include "fspp/filesystem.hpp"

#include <string>


namespace eyestep {

class Sosofo;
class IFormattingObject;

class IProcessor
{
public:
  virtual ~IProcessor() {}

  virtual void set_output_file(const filesystem::path& output_file) = 0;
  virtual std::string default_output_extension() const = 0;
  virtual void add_program_options(cxxopts::Options& options) const = 0;

  virtual std::string proc_id() const = 0;
  virtual void render_processed_node(const Sosofo* sosofo) = 0;

  virtual void render_sosofo(const Sosofo* sosofo) = 0;
  virtual void render_fo(const IFormattingObject* fo) = 0;
};

} // namespace eyestep
