// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fo.hpp"
#include "processor.hpp"
#include "propstack.hpp"

#include "fspp/filesystem.hpp"

#include <boost/optional/optional.hpp>

#include <string>


namespace eyestep {

class Sosofo;
class IFormattingObject;
template <typename T>
class IFoProcessor;


template <typename ProcessorT>
class AbstractProcessor : public IProcessor {
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

  fo::PropertySpecOrNone property(const IFormattingObject* fo,
                                  const std::string& key) const override;

  template <typename T>
  T property(const IFormattingObject* fo, const std::string& key,
             T default_value) const;

  template <typename T>
  boost::optional<T> property_or_none(const IFormattingObject* fo,
                                      const std::string& key) const;
};

} // ns eyestep

#include "abstract-processor.ipp"
