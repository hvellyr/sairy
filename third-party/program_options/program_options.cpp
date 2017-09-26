#include "program_options.hpp"

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

const std::regex
  option_specifier("([[:alnum:]][-_[:alnum:]]+)?(?:,([[:alnum:]]))?");
const std::regex long_option_matcher("--([[:alnum:]][-_[:alnum:]]+)(?:=(.*))?");


option_description::option_description(
  const std::string& opts_, const std::shared_ptr<const value_semantic>& sm_,
  const std::string& desc_, const std::string& arg_, bool hide_)
  : semantic(sm_), desc(desc_), arg(arg_), hide(hide_)
{
  std::smatch matches;
  regex_match(opts_, matches, option_specifier);

  l = std::move(matches[1]);
  s = std::move(matches[2]);

  if (s.empty() && l.empty())
    throw bad_option_spec(opts_);
}


positional_options_description&
positional_options_description::add(const std::string& name, int max_count)
{
  if (max_count < 0)
    max_count = (~0U);

  if (max_total_count_ + max_count > max_total_count_)
    max_total_count_ += max_count;

  set_.insert(name);

  positionals_.emplace_back(strip_prefix(name), (unsigned)max_count);

  return *this;
}

const std::string&
positional_options_description::name_for_position(unsigned pos) const
{
  if (pos >= max_total_count_)
    return empty_name_;

  for (const auto& pe : positionals_) {
    if (pos < pe.second)
      return pe.first;
    pos -= pe.second;
  }

  return empty_name_;
}

options_description::options_description(const std::string& caption)
{
  option_group optgroup;
  optgroup.caption_ = caption;
  groups_.push_back(optgroup);
}

void options_description::add_option(
  const std::shared_ptr<const option_description>& desc)
{
  const auto& s = desc->s;
  const auto& l = desc->l;

  if (!s.empty() && groups_.front().optset_.count(s))
    throw duplicate_option(s);

  if (!l.empty() && groups_.front().optset_.count(l))
    throw duplicate_option(l);

  groups_.front().options_.push_back(desc);
}


options_description&
options_description::add(const options_description& options)
{
  for (const auto group : options.groups_) {
    groups_.push_back(group);
  }

  return *this;
}


bool options_description::empty() const
{
  return std::accumulate(begin(groups_), end(groups_), 0,
                         [](int sum, const option_group& group) {
                           return sum + group.options_.size();
                         }) == 0;
}


std::string options_description::help() const
{
  std::string desc;
  desc += "Options:\n";

  for (const auto group : groups_) {
    if (!group.caption_.empty()) {
      desc += "\n" + group.caption_ + ":\n";
    }

    size_t longest = 0;

    std::vector<std::string> opts;

    for (const auto& o : group.options_) {
      if (o->hide)
        continue;

      const auto& s = format_opt(o);
      longest = std::max(longest, s.length());
      opts.push_back(s);
    }

    longest = std::min(longest, OPTION_LONGEST);

    // widest allowed description
    size_t allowed = OPTION_LINE_LENGTH - longest - OPTION_DESC_GAP;

    auto it = opts.cbegin();

    for (const auto& o : group.options_) {
      if (o->hide)
        continue;

      desc += *it;

      if (it->length() > longest) {
        desc += '\n';
        desc.append(longest + OPTION_DESC_GAP, ' ');
      }
      else {
        desc.append(longest + OPTION_DESC_GAP - it->length(), ' ');
      }

      desc += format_desc(o, longest + OPTION_DESC_GAP, allowed);
      desc += '\n';

      ++it;
    }
  }

  return desc;
}

std::string options_description::format_opt(
  const std::shared_ptr<const option_description>& o) const
{
  std::string result = "  ";

  if (!o->s.empty())
    result += "-" + o->s + ",";
  else
    result += "   ";

  if (!o->l.empty())
    result += " --" + o->l;

  const auto& sm = o->semantic;

  if (sm->has_arg()) {
    const auto& arg = o->arg.empty() ? "arg" : o->arg;

    if (sm->has_implicit())
      result += " [" + arg + "(=" + sm->implicit_str() + ")]";
    else
      result += " <" + arg + ">";
  }

  return result;
}

std::string options_description::format_desc(
  const std::shared_ptr<const option_description>& o, size_t indent,
  size_t width) const
{
  std::string desc = o->desc;

  const auto& sm = o->semantic;

  if (sm->is_required())
    desc += " (required)";

  if (sm->has_default())
    desc += " (default: " + sm->default_str() + ")";

  std::string result;

  auto cur = std::begin(desc);
  auto stp = cur;
  auto lsp = cur;

  for (size_t size = 0; cur != std::end(desc); ++cur) {
    if (*cur == ' ')
      lsp = cur;

    if (++size > width) {
      if (lsp > stp) {
        result.append(stp, lsp);
        result.push_back('\n');
        result.append(indent, ' ');
        stp = lsp + 1;
      }
      else {
        result.append(stp, cur + 1);
        result.push_back('\n');
        result.append(indent, ' ');
        stp = cur + 1;
        lsp = stp;
      }
      size = 0;
    }
  }

  // append whatever is left
  result.append(stp, cur);

  return result;
}

parsed_options
parse_command_line(int argc, const char* const argv[],
                   const options_description& desc,
                   const positional_options_description& pos_desc)
{
  parsed_options results;

  for (const auto& group : desc.groups_) {
    for (const auto& opt : group.options_) {
      std::shared_ptr<variable_value> value = opt->semantic->create_value();
      bool pos = false;

      if (opt->s.empty() && opt->l.empty()) {
        results.emplace(opt->s, value);
        pos = true;
      }
      else {
        if (!opt->s.empty()) {
          results.emplace(opt->s, value);
          pos |= pos_desc.contains(opt->s);
        }
        if (!opt->l.empty()) {
          results.emplace(opt->l, value);
          pos |= pos_desc.contains(opt->l);
        }
      }

      value->is_pos() = pos;
    }
  }

  auto is_option = [](const char* arg) -> bool {
    /* starts  with '-' (and is not exactly "-" or "--") */
    if (arg[0] == '-')
      return (arg[1] && (arg[1] != '-' || arg[2]));

    return false;
  };

  auto find_variable_value =
    [&results](const std::string& opt) -> std::shared_ptr<variable_value> {
    const auto& it = results.find(opt);
    if (it == results.end())
      throw unknown_option(opt);

    return it->second;
  };

  auto get_next_arg =
    [&argc, &argv,
     &is_option](const std::string& opt,
                 const std::shared_ptr<const value_semantic>& semantic,
                 int& idx) -> const char* {
    if (idx + 1 >= argc || is_option(argv[idx + 1]) ||
        strcmp(argv[idx + 1], "--") == 0) {
      if (semantic->has_implicit())
        return "";
      throw missing_argument(opt);
    }

    return argv[++idx];
  };

  size_t pos_n = 0;

  auto consume_positional = [&](const char* arg) {
    const auto& opt = pos_desc.name_for_position(pos_n);
    const auto& value = find_variable_value(opt);
    const auto& semantic = value->semantic();

    if (!semantic->has_arg())
      throw superfluous_argument(opt, arg);

    semantic->parse(arg, value);
    ++pos_n;
  };

  for (int ai = 1; ai < argc; ++ai) {
    if (strcmp(argv[ai], "--") == 0) {
      while (++ai < argc)
        consume_positional(argv[ai]);
      break;
    }

    /* is it an option? */
    if (is_option(argv[ai])) {
      if (argv[ai][1] == '-') { /* a long option */
        std::cmatch matches;
        regex_match(argv[ai], matches, long_option_matcher);

        if (matches.empty())
          throw invalid_option_name(argv[ai]);

        std::string opt = std::move(matches[1]);
        std::string arg = std::move(matches[2]);

        const auto& value = find_variable_value(opt);
        const auto& semantic = value->semantic();

        if (!arg.empty() && !semantic->has_arg())
          throw superfluous_argument(opt, arg);

        if (arg.empty() && semantic->has_arg())
          arg = std::string(get_next_arg(opt, semantic, ai));

        semantic->parse(arg, value);
      }
      else { /* short option(s) */
        for (int i = 1; argv[ai][i]; ++i) {
          const auto& opt = std::string(1, argv[ai][i]);
          const auto& value = find_variable_value(opt);
          const auto& semantic = value->semantic();

          if (semantic->has_arg()) {
            std::string arg;

            if (argv[ai][i + 1])
              arg = std::string(&argv[ai][i + 1]);
            else
              arg = std::string(get_next_arg(opt, semantic, ai));

            semantic->parse(arg, value);
            break;
          }

          semantic->parse("", value);
        }
      }
    }
    else { /* not an option flag */
      consume_positional(argv[ai]);
    }
  }

  for (const auto& res : results)
    res.second->semantic()->parse(res.second);

  return results;
}

void store(parsed_options&& options, variables_map& vm)
{
  vm.values_ = std::move(options);
}

void notify(variables_map& vm)
{
  for (const auto& vp : vm.values_)
    if (vp.second->count() == 0 && vp.second->semantic()->is_required())
      throw missing_option(vp.first, vp.second->is_pos());

  for (const auto& vp : vm.values_)
    if (vp.second->count())
      vp.second->notify();
}
}
