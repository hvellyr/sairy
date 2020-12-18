// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "config.hpp"

#include "casefold.hpp"
#include "utils.hpp"

#include "fspp/filesystem.hpp"

#ifdef TEXTBOOK_HAVE_STD_CODECVT
#include <codecvt>
#endif

#include <algorithm>
#include <cctype>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>


namespace eyestep {
namespace utils {

  namespace fs = eyestep::filesystem;

  std::string join(const std::vector<std::string>& strlist, const std::string& gap) {
    using namespace std;

    stringstream result;

    if (!strlist.empty()) {
      result << strlist.front();

      for_each(next(begin(strlist)), end(strlist),
               [&](const string& str) { result << gap << str; });
    }

    return result.str();
  }


  std::vector<std::string> join_list(const std::vector<std::string>& one,
                                     const std::vector<std::string>& sec) {
    using namespace std;

    auto result = vector<string>{};
    result.insert(end(result), begin(one), end(one));
    result.insert(end(result), begin(sec), end(sec));
    return result;
  }


  std::vector<std::string> split(const std::string& str, const std::string& seps,
                                 bool trim_token) {
    using namespace std;

    const auto push_substr = [&](vector<string>& res, const std::string& src, size_t p,
                                 size_t count) {
      const auto tmp = src.substr(p, count);
      res.emplace_back(trim_token ? trim_copy(tmp) : tmp);
    };

    auto result = vector<string>{};

    auto pos = std::size_t{0};

    pos = str.find_first_not_of(seps, pos);
    if (pos == std::string::npos) {
      return result;
    }

    while (pos < str.size()) {
      auto idx = str.find_first_of(seps, pos);
      if (idx == std::string::npos) {
        push_substr(result, str, pos, std::string::npos);
        return result;
      }

      push_substr(result, str, pos, idx - pos);

      idx = str.find_first_not_of(seps, idx);
      if (idx == std::string::npos) {
        return result;
      }
      pos = idx;
    }

    return result;
  }


  std::vector<std::string> split_str(const std::string& str, const std::string& substr,
                                     bool trim_token) {
    using namespace std;

    const auto push_substr = [&](vector<string>& res, const std::string& src, size_t p,
                                 size_t count) {
      const auto tmp = src.substr(p, count);
      res.emplace_back(trim_token ? trim_copy(tmp) : tmp);
    };

    auto result = vector<string>{};

    auto pos = std::size_t{0};

    while (pos < str.size()) {
      auto idx = str.find(substr, pos);
      if (idx == std::string::npos) {
        push_substr(result, str, pos, std::string::npos);
        return result;
      }

      push_substr(result, str, pos, idx - pos);

      pos = idx + substr.size();
    }

    return result;
  }


  std::string replace_str(const std::string& src, const std::string& pattern,
                          const std::string& replcm) {
    return join(split_str(src, pattern, true), replcm);
  }


  std::vector<fs::path> split_paths(const std::string& path) {
    using namespace std;

    auto components = split(path, ":", true);

    auto result = vector<fs::path>{};
    result.reserve(components.size());

    transform(begin(components), end(components), back_inserter(result),
              [](const string& val) { return fs::path{val}; });

    return result;
  }


  // Return path when appended to a_From will resolve to same as a_To
  fs::path make_relative(const fs::path& from, const fs::path& to) {
    auto _from = fs::absolute(from);
    auto _to = fs::absolute(to);

    auto ret = fs::path{};
    auto i_from = _from.begin();
    auto i_to = _to.begin();

    // Find common base
    for (auto to_end = _to.end(), from_end = _from.end();
         i_from != from_end && i_to != to_end && *i_from == *i_to; ++i_from, ++i_to) {
    }
    // Navigate backwards in directory to reach previously found base
    for (auto from_end = _from.end(); i_from != from_end; ++i_from) {
      if ((*i_from) != ".") {
        ret /= "..";
      }
    }
    // Now navigate down the directory branch
    for (; i_to != _to.end(); ++i_to) {
      ret /= *i_to;
    }
    // ret.append(i_to, _to.end());
    return ret;
  }


//------------------------------------------------------------------------------

#ifdef TEXTBOOK_HAVE_STD_CODECVT
  std::u32string utf8_to_u32string(const std::string& str) {
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> conv32;
    return conv32.from_bytes(str);
  }

  std::string u32string_to_utf8(const std::u32string& str) {
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> conv32;
    return conv32.to_bytes(str);
  }

#else

  std::u32string utf8_to_u32string(const std::string& src) {
    using namespace std;

    auto tmp = vector<char32_t>{};
    tmp.reserve(src.length());

    auto i_src = begin(src);
    auto i_end = end(src);

    while (i_src != i_end) {
      if (!(*i_src & 0x80)) {
        tmp.emplace_back(static_cast<char32_t>(*i_src));
        ++i_src;
      }
      else {
        auto d = char32_t(0xfffd);

        if ((*i_src & 0xe0) == 0xc0) {
          if (distance(i_src, i_end) >= 1) {
            d = static_cast<char32_t>(((static_cast<char32_t>(*i_src) & 0x1F) << 6) |
                                      (static_cast<char32_t>(*next(i_src)) & 0x3F));
            advance(i_src, 2);
          }
          else
            throw runtime_error("incomplete utf8 character sequence");
        }
        else if ((*i_src & 0xF0) == 0xE0) {
          if (distance(i_src, i_end) >= 2) {
            d =
              static_cast<char32_t>(((static_cast<char32_t>(*i_src) & 0x0F) << 12) |
                                    ((static_cast<char32_t>(*next(i_src)) & 0x3F) << 6) |
                                    (static_cast<char32_t>(*next(i_src, 2)) & 0x3F));
            advance(i_src, 3);
          }
          else
            throw runtime_error("incomplete utf8 character sequence");
        }
        else if ((*i_src & 0xF8) == 0xF0) {
          if (distance(i_src, i_end) >= 3) {
            d = static_cast<char32_t>(
              ((static_cast<char32_t>(*i_src) & 0x07) << 18) |
              ((static_cast<char32_t>(*next(i_src)) & 0x3F) << 12) |
              ((static_cast<char32_t>(*next(i_src, 2)) & 0x3F) << 6) |
              ((static_cast<char32_t>(*next(i_src, 3)) & 0x3F)));
            advance(i_src, 4);
          }
          else
            throw runtime_error("incomplete utf8 character sequence");
        }
        else {
          throw runtime_error("invalid utf8 character sequence");
        }

        tmp.emplace_back(d);
      }
    }

    return {begin(tmp), end(tmp)};
  }

  std::string u32string_to_utf8(const std::u32string& src) {
    using namespace std;

    auto tmp = vector<char>{};
    tmp.reserve(src.length());

    for (const auto c : src) {
      if (c & 0xff80) {
        if (c & 0xf800) {
          tmp.emplace_back(static_cast<unsigned char>(0xe0 | (c >> 12)));
          tmp.emplace_back(static_cast<unsigned char>(0x80 | ((c >> 6) & 0x3f)));
        }
        else {
          tmp.emplace_back(static_cast<unsigned char>(0xc0 | ((c >> 6) & 0x3f)));
        }

        tmp.emplace_back(static_cast<unsigned char>(0x80 | (c & 0x3f)));
      }
      else {
        tmp.emplace_back(static_cast<unsigned char>(c));
      }
    }

    return {begin(tmp), end(tmp)};
  }

#endif

  std::string to_lower(const std::string& src) {
    using namespace std;

    auto u32src = utf8_to_u32string(src);
    auto u32dst = u32string{};

    u32dst.reserve(u32src.size());

    transform(begin(u32src), end(u32src), back_inserter(u32dst),
              [](const char32_t c) { return utils::towlower(c); });

    return u32string_to_utf8(u32dst);
  }


  std::string to_upper(const std::string& src) {
    using namespace std;

    auto u32src = utf8_to_u32string(src);
    auto u32dst = u32string{};

    u32dst.reserve(u32src.size());

    transform(begin(u32src), end(u32src), back_inserter(u32dst),
              [](const char32_t c) { return utils::towupper(c); });

    return u32string_to_utf8(u32dst);
  }


  bool iswspace(char32_t c) {
    return (c >= 0x0009 && c <= 0x000d)    // <control-0009>..<control-000D>
           || c == 0x0020                  // SPACE
           || c == 0x0085                  // <control-0085>
           || c == 0x00a0                  // NO-BREAK SPACE
           || c == 0x1680                  // OGHAM SPACE MARK
           || (c >= 0x2000 && c <= 0x200a) // EN QUAD..HAIR SPACE
           || c == 0x2028                  // LINE SEPARATOR
           || c == 0x2029                  // PARAGRAPH SEPARATOR
           || c == 0x202f                  // NARROW NO-BREAK SPACE
           || c == 0x205f                  // MEDIUM MATHEMATICAL SPACE
           || c == 0x3000;                 // IDEOGRAPHIC SPACE
  }


  std::u32string& trim_left(std::u32string& src) {
    using namespace std;
    src.erase(begin(src), std::find_if_not(begin(src), end(src),
                                           [](const char32_t c) { return iswspace(c); }));
    return src;
  }


  std::u32string& trim_right(std::u32string& src) {
    using namespace std;
    src.erase(std::find_if_not(src.rbegin(), src.rend(),
                               [](const char32_t c) { return iswspace(c); })
                .base(),
              src.end());
    return src;
  }


  std::u32string& trim(std::u32string& src) {
    trim_left(src);
    trim_right(src);
    return src;
  }


  std::string trim_left_copy(const std::string& src) {
    auto u32src = utf8_to_u32string(src);
    return u32string_to_utf8(trim_left(u32src));
  }


  std::string trim_right_copy(const std::string& src) {
    auto u32src = utf8_to_u32string(src);
    return u32string_to_utf8(trim_right(u32src));
  }


  std::string trim_copy(const std::string& src) {
    auto u32src = utf8_to_u32string(src);
    return u32string_to_utf8(trim(u32src));
  }

} // ns utils
} // ns eyestep
