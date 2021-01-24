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

class IFormattingObject;
template <typename T>
class IFoProcessor;
class Sosofo;
class StyleEngine;


namespace detail {
  class PropertyLookup : public fo::IProperties
  {
    const PropertiesStack* _props = nullptr;

  public:
    PropertyLookup() = default;
    PropertyLookup(const PropertiesStack* props)
      : _props(props) {}
    PropertyLookup(const PropertyLookup& rhs) = default;
    PropertyLookup(PropertyLookup&& rhs) = default;
    PropertyLookup& operator=(const PropertyLookup& rhs) = default;
    PropertyLookup& operator=(PropertyLookup&& rhs) = default;

    fo::ValueType get(const std::string& key) const override;
    fo::ValueType get_default(const std::string& key) const override;
  };

} // namespace detail


template <typename ProcessorT>
class AbstractProcessor : public IProcessor
{
protected:
  filesystem::path _output_file;
  PropertiesStack _props;
  detail::PropertyLookup _prop_lookup;
  StyleEngine* _engine = nullptr;

public:
  AbstractProcessor();

  void set_output_file(const filesystem::path& output_file) override;
  void render_processed_node(StyleEngine* engine, const Sosofo* sosofo) override;

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

  StyleEngine* engine();
};

} // namespace eyestep

#include "abstract-processor.ipp"
