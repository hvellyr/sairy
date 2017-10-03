#ifndef _PROGRAM_OPTIONS_HPP
#define _PROGRAM_OPTIONS_HPP

#include <cstring>
#include <functional>
#include <map>
#include <memory>
#include <numeric>
#include <regex>
#include <set>
#include <sstream>
#include <vector>


namespace program_options {


inline const std::string with_prefix(const std::string& opt)
{
  return (opt.length() > 1 && opt[0] != '-') ? "--" + opt : opt;
}


class option_error : public std::exception {
public:
  option_error(const std::string& name, const std::string& value,
               const std::string& message)
    : name_(name), value_(value), message_(message)
  {
  }

  std::string name() const { return name_; }
  std::string value() const { return value_; }

  virtual const char* what() const noexcept { return message_.c_str(); }

protected:
  std::string name_;
  std::string value_;
  std::string message_;
};


class option_desc_error : public option_error {
public:
  using option_error::option_error;
};


class duplicate_option : public option_desc_error {
public:
  explicit duplicate_option(const std::string& name)
    : option_desc_error(name, "", "duplicate option '" + name + "'")
  {
  }
};


class bad_option_spec : public option_desc_error {
public:
  explicit bad_option_spec(const std::string& spec)
    : option_desc_error(spec, "", "bad option specifier '" + spec + "'")
  {
  }
};


class option_parse_error : public option_error {
public:
  using option_error::option_error;
};


class invalid_option_name : public option_parse_error {
public:
  explicit invalid_option_name(const std::string& name)
    : option_parse_error(name, "", "invalid option '" + name + "'")
  {
  }
};


class unknown_option : public option_parse_error {
public:
  explicit unknown_option(const std::string& opt)
    : option_parse_error(opt, "", "unknown option '" + with_prefix(opt) + "'")
  {
  }
};


class superfluous_argument : public option_parse_error {
public:
  superfluous_argument(const std::string& opt, const std::string& arg)
    : option_parse_error(opt, arg, "unnecessary argument '" + arg +
                                     "' for option '" + with_prefix(opt) + "'")
  {
  }
};


class missing_argument : public option_parse_error {
public:
  explicit missing_argument(const std::string& opt)
    : option_parse_error(opt, "", "missing argument for option '" +
                                    with_prefix(opt) + "'")
  {
  }
};


class invalid_argument : public option_parse_error {
public:
  invalid_argument(const std::string& opt, const std::string& arg)
    : option_parse_error(opt, arg, "invalid argument '" + arg + "'")
  {
    if (!opt.empty())
      message_ += " for option '" + with_prefix(opt) + "'";
  }
};


class invalid_bool_value : public option_parse_error {
public:
  explicit invalid_bool_value(const std::string& value)
    : option_parse_error("", value, "invalid bool value '" + value +
                                      "', accepts: (yes|no)")
  {
  }
};


class missing_option : public option_parse_error {
public:
  explicit missing_option(const std::string& opt, bool is_pos = false)
    : option_parse_error(opt, "", "")
  {
    if (is_pos)
      message_ = "missing " + opt;
    else
      message_ = "missing option '" + with_prefix(opt) + "'";
  }
};


class option_map_error : public option_error {
public:
  using option_error::option_error;
};


class option_not_present : public option_map_error {
public:
  explicit option_not_present(const std::string& opt)
    : option_map_error(opt, "",
                       "option '" + with_prefix(opt) + "' is not present")
  {
  }
};


class variable_value;

class value_semantic {
public:
  virtual bool has_arg() const = 0;
  virtual bool has_default() const = 0;
  virtual bool has_implicit() const = 0;
  virtual bool is_vector() const = 0;
  virtual bool is_required() const = 0;

  virtual std::string default_str() const = 0;
  virtual std::string implicit_str() const = 0;

  virtual std::shared_ptr<variable_value> create_value() const = 0;

  virtual void parse(const std::shared_ptr<variable_value>& val) const = 0;
  virtual void parse(const std::string& text,
                     const std::shared_ptr<variable_value>& val) const = 0;

  virtual void notify(const variable_value&) const = 0;
};


class variable_value {
public:
  variable_value(const std::shared_ptr<const value_semantic>& semantic)
    : typeid_(typeid(void)), semantic_(semantic)
  {
  }

  template <typename T>
  variable_value(const std::shared_ptr<const value_semantic>& semantic,
                 T* store)
    : typeid_(typeid(T)), semantic_(semantic)
  {
    if (store)
      value_ = store;
    else {
      holder_ = std::make_shared<T>();
      value_ = holder_.get();
    }
  }

  template <typename T>
  const T& as() const
  {
    if (typeid_ != typeid(T))
      throw std::bad_cast();
    return *(static_cast<const T*>(value_));
  }

  template <typename T>
  T& as()
  {
    if (typeid_ != typeid(T))
      throw std::bad_cast();
    return *(static_cast<T*>(value_));
  }

  void notify() { semantic_->notify(*this); }

  const std::shared_ptr<const value_semantic> semantic() const
  {
    return semantic_;
  }

  size_t& count() { return count_; }

  bool& is_pos() { return is_pos_; }

private:
  const std::type_info& typeid_;
  std::shared_ptr<const value_semantic> semantic_;
  std::shared_ptr<void> holder_;
  void* value_ = nullptr;
  size_t count_ = 0;
  bool is_pos_ = false;
};


template <typename T>
struct type_is_vector {
  static constexpr bool value = false;
};


template <typename T>
struct type_is_vector<std::vector<T>> {
  static constexpr bool value = true;
};


template <typename T>
void parse_value(const std::string& text, T& value)
{
  std::istringstream iss(text);

  if (!(iss >> value))
    throw invalid_argument("", text);

  if (iss.rdbuf()->in_avail() != 0)
    throw invalid_argument("", text);
}


inline void parse_value(const std::string& text, std::string& value)
{
  value = text;
}


inline void parse_value(const std::string& text, bool& value)
{
  if (!strcasecmp(text.c_str(), "yes"))
    value = true;
  if (!strcasecmp(text.c_str(), "no"))
    value = false;
  else
    throw invalid_bool_value(text);
}


template <typename T>
void parse_value(const std::string& text, std::vector<T>& value)
{
  T v;
  parse_value(text, v);
  value.push_back(v);
}


template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& vec)
{
  os << "[";
  if (vec.size() > 0) {
    os << vec[0];
    for (size_t i = 1; i < vec.size(); ++i)
      os << ", " << vec[i];
  }
  os << "]";
  return os;
}


template <typename T>
class option_value : public value_semantic,
                     public std::enable_shared_from_this<option_value<T>> {
public:
  option_value() = default;

  option_value(T* t) : store_(t) {}

  bool has_arg() const override { return true; }
  bool has_default() const override { return default_ ? true : false; }
  bool has_implicit() const override { return implicit_ ? true : false; }
  bool is_vector() const override { return type_is_vector<T>::value; }
  bool is_required() const override { return required_; }

  std::string default_str() const override
  {
    std::ostringstream oss;
    oss << *default_;
    return oss.str();
  }

  std::string implicit_str() const override
  {
    std::ostringstream oss;
    oss << *implicit_;
    return oss.str();
  }

  std::shared_ptr<variable_value> create_value() const override
  {
    return std::make_shared<variable_value>(this->shared_from_this(), store_);
  }

  void parse(const std::shared_ptr<variable_value>& val) const override
  {
    if (val->count() == 0 && default_) {
      val->as<T>() = *default_;
      val->count()++;
    }
  }

  void parse(const std::string& text,
             const std::shared_ptr<variable_value>& val) const override
  {
    if (text.empty() && implicit_)
      val->as<T>() = *implicit_;
    else
      parse_value(text, val->as<T>());

    val->count()++;
  }

  void notify(const variable_value& val) const override
  {
    if (notifier_)
      notifier_(val.as<T>());
  }

  std::shared_ptr<option_value<T>> default_value(const T& value)
  {
    default_ = std::make_shared<const T>(value);
    return this->shared_from_this();
  }

  std::shared_ptr<option_value<T>> implicit_value(const T& value)
  {
    implicit_ = std::make_shared<const T>(value);
    return this->shared_from_this();
  }

  std::shared_ptr<option_value<T>>
  notifier(const std::function<void(const T&)>& fn)
  {
    notifier_ = fn;
    return this->shared_from_this();
  }

  std::shared_ptr<option_value<T>> required()
  {
    required_ = true;
    return this->shared_from_this();
  }

private:
  T* const store_ = nullptr;
  std::shared_ptr<const T> default_;
  std::shared_ptr<const T> implicit_;
  std::function<void(const T&)> notifier_;
  bool required_ = false;
};


template <>
class option_value<void>
  : public value_semantic,
    public std::enable_shared_from_this<option_value<void>> {
public:
  bool has_arg() const override { return false; }
  bool has_default() const override { return false; }
  bool has_implicit() const override { return false; }
  bool is_vector() const override { return false; }
  bool is_required() const override { return required_; }

  std::string default_str() const override { return ""; }
  std::string implicit_str() const override { return ""; }

  std::shared_ptr<variable_value> create_value() const override
  {
    return std::make_shared<variable_value>(this->shared_from_this());
  }

  void parse(const std::shared_ptr<variable_value>& /*val*/) const override {}

  void parse(const std::string& /* text */,
             const std::shared_ptr<variable_value>& val) const override
  {
    val->count()++;
  }

  void notify(const variable_value& /*val*/) const override
  {
    if (notifier_)
      notifier_();
  }

  std::shared_ptr<option_value<void>>
  notifier(const std::function<void(void)>& fn)
  {
    notifier_ = fn;
    return this->shared_from_this();
  }

  std::shared_ptr<option_value<void>> required()
  {
    required_ = true;
    return this->shared_from_this();
  }

private:
  std::function<void(void)> notifier_;
  bool required_ = false;
};


template <typename T>
std::shared_ptr<option_value<T>> value()
{
  return std::make_shared<option_value<T>>();
}


template <typename T>
std::shared_ptr<option_value<T>> value(T* t)
{
  return std::make_shared<option_value<T>>(t);
}


struct option_description {
  option_description()
    : semantic(std::make_shared<option_value<std::vector<std::string>>>()),
      hide(true)
  {
  }

  option_description(const std::string& opts_,
                     const std::shared_ptr<const value_semantic>& sm_,
                     const std::string& desc_, const std::string& arg_,
                     bool hide_);

  std::string s;
  std::string l;
  std::shared_ptr<const value_semantic> semantic;
  std::string desc;
  std::string arg;
  bool hide;
};


class options_description;

class option_adder {
public:
  option_adder(options_description* owner) : owner_(owner) {}

  option_adder& operator()(const std::string& opts, const std::string& desc)
  {
    return (*this)(opts, value<void>(), desc);
  }

  option_adder&
  operator()(const std::string& opts,
             const std::shared_ptr<const value_semantic>& semantic,
             const std::string& desc = "", const std::string& arg = "",
             bool hide = false);

private:
  options_description* owner_;
};


class positional_options_description {
public:
  positional_options_description& add(const std::string& name, int max_count);

  const std::string& name_for_position(unsigned pos) const;

  bool contains(const std::string& name) const { return set_.count(name); }

private:
  const std::string empty_name_ = "";
  std::vector<std::pair<std::string, unsigned>> positionals_;
  std::set<std::string> set_;
  unsigned max_total_count_ = 0;
};


using parsed_options = std::map<std::string, std::shared_ptr<variable_value>>;


class options_description {
  friend parsed_options
  parse_command_line(int argc, const char* const argv[],
                     const options_description& desc,
                     const positional_options_description& pos_desc);

  friend std::ostream& operator<<(std::ostream& os,
                                  const options_description& desc);

public:
  options_description(const std::string& caption);

  option_adder add_options() { return option_adder(this); }

  void add_option(const std::shared_ptr<const option_description>& desc);

  options_description& add(const options_description& options);
  bool empty() const;

private:
  std::string help() const;

  std::string
  format_opt(const std::shared_ptr<const option_description>& o) const;

  std::string format_desc(const std::shared_ptr<const option_description>& o,
                          size_t indent, size_t width) const;

  const size_t OPTION_LINE_LENGTH = 76;
  const size_t OPTION_LONGEST = 30;
  const size_t OPTION_DESC_GAP = 2;

  struct option_group {
    std::string caption_;
    std::set<std::string> optset_;
    std::vector<std::shared_ptr<const option_description>> options_;
  };

  std::vector<option_group> groups_;
};


inline std::ostream& operator<<(std::ostream& os,
                                const options_description& desc)
{
  return os << desc.help();
}


class variables_map {
  friend void store(parsed_options&& options, variables_map& vm);
  friend void notify(variables_map& vm);

public:
  size_t count(const std::string& opt) const;
  bool empty() const { return values_.empty(); }

  const variable_value& operator[](const std::string& opt) const;

private:
  parsed_options values_;
};


inline const std::string strip_prefix(const std::string& opt)
{
  size_t i = 0;
  while (i < 2 && i < opt.length() && opt[i] == '-')
    ++i;
  return i > 0 ? opt.substr(i) : opt;
}


inline option_adder& option_adder::
operator()(const std::string& opts,
           const std::shared_ptr<const value_semantic>& semantic,
           const std::string& desc, const std::string& arg, bool hide)
{
  owner_->add_option(std::make_shared<const option_description>(opts, semantic,
                                                                desc, arg,
                                                                hide));

  return *this;
}


inline size_t variables_map::count(const std::string& opt) const
{
  const auto& it = values_.find(strip_prefix(opt));
  if (it == values_.end())
    return 0;
  return it->second->count();
}


inline const variable_value& variables_map::
operator[](const std::string& opt) const
{
  const auto& it = values_.find(strip_prefix(opt));
  if (it == values_.end())
    throw option_not_present(opt);
  if (it->second->count() == 0)
    throw option_not_present(opt);
  return *(it->second);
}


parsed_options
parse_command_line(int argc, const char* const argv[],
                   const options_description& desc,
                   const positional_options_description& pos_desc =
                     positional_options_description());

void store(parsed_options&& options, variables_map& vm);

void notify(variables_map& vm);
}

#endif
