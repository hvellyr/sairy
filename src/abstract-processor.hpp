// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fo.hpp"
#include "processor.hpp"
#include "propstack.hpp"

#include "fspp/estd/optional.hpp"
#include "fspp/filesystem.hpp"

#include <string>


namespace eyestep {

class Sosofo;
class IFormattingObject;
template <typename T>
class IFoProcessor;


template <typename ProcessorT>
class AbstractProcessor : public IProcessor
{
protected:
  filesystem::path _output_file;
  PropertiesStack _props;

public:
  void set_output_file(const filesystem::path& output_file) override;
  void render_processed_node(const Sosofo* sosofo) override;

  void render_sosofo(const Sosofo* sosofo) override;
  void render_fo(const IFormattingObject* fo) override;

  virtual const IFoProcessor<ProcessorT>*
  lookup_fo_processor(const std::string& fo_class_name) const = 0;

  virtual void before_rendering();
  virtual void after_rendering();

  template <typename T>
  estd::optional<T> property_or_none(const std::string& key) const;
  template <typename T>
  T property(const std::string& key, T default_value) const;
};

} // namespace eyestep

#include "abstract-processor.ipp"
