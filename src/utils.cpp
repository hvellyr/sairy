// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "config.hpp"

#include "utils.hpp"
#include "casefold.hpp"

#include "fspp/filesystem.hpp"

#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/iterator.hpp>
#include <boost/range/iterator_range.hpp>

#ifdef HAVE_STD_CODECVT
#include <codecvt>
#else
#include <boost/locale/encoding_utf.hpp>
#endif

#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>


namespace eyestep {
namespace utils {

  namespace fs = eyestep::filesystem;

  std::string join(const std::vector<std::string>& strlist,
                   const std::string& gap)
  {
    std::stringstream result;

    if (!strlist.empty()) {
      result << strlist.front();

      for (const auto& str :
           boost::make_iterator_range(boost::next(strlist.begin()),
                                      strlist.end())) {
        result << gap << str;
      }
    }

    return result.str();
  }


  std::vector<std::string> join_list(const std::vector<std::string>& one,
                                     const std::vector<std::string>& sec)
  {
    std::vector<std::string> result;
    result.insert(result.end(), one.begin(), one.end());
    result.insert(result.end(), sec.begin(), sec.end());
    return result;
  }


  std::vector<fs::path> split_paths(const std::string& path)
  {
    using namespace boost::adaptors;
    using namespace boost::algorithm;

    std::vector<std::string> steps;
    split(steps, path, is_any_of(":"), token_compress_on);

    return boost::copy_range<std::vector<fs::path>>(
      steps |
      transformed([](const std::string& value) { return fs::path(value); }));
  }


  // Return path when appended to a_From will resolve to same as a_To
  fs::path make_relative(const fs::path& from, const fs::path& to)
  {
    auto _from = fs::absolute(from);
    auto _to = fs::absolute(to);

    fs::path ret;
    fs::path::const_iterator i_from(_from.begin());
    fs::path::const_iterator i_to(_to.begin());

    // Find common base
    for (fs::path::const_iterator to_end(_to.end()), from_end(_from.end());
         i_from != from_end && i_to != to_end && *i_from == *i_to;
         ++i_from, ++i_to) {
    }
    // Navigate backwards in directory to reach previously found base
    for (fs::path::const_iterator from_end(_from.end()); i_from != from_end;
         ++i_from) {
      if ((*i_from) != ".") {
        ret /= "..";
      }
    }
    // Now navigate down the directory branch
    for ( ; i_to != _to.end() ; ++i_to) {
      ret /= *i_to;
    }
    //ret.append(i_to, _to.end());
    return ret;
  }


  //------------------------------------------------------------------------------

#ifdef HAVE_STD_CODECVT
  std::u16string utf8_to_u16string(const std::string& str)
  {
    std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> conv16;
    return conv16.from_bytes(str);
  }

  std::string u16string_to_utf8(const std::u16string& str)
  {
    std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> conv8;
    return conv8.to_bytes(str);
  }

  std::u32string utf8_to_u32string(const std::string& str)
  {
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> conv32;
    return conv32.from_bytes(str);
  }

  std::string u32string_to_utf8(const std::u32string& str)
  {
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> conv32;
    return conv32.to_bytes(str);
  }

#else

  std::u16string utf8_to_u16string(const std::string& str)
  {
    using boost::locale::conv::utf_to_utf;
    return utf_to_utf<char16_t>(str.c_str(), str.c_str() + str.size());
  }

  std::string u16string_to_utf8(const std::u16string& str)
  {
    using boost::locale::conv::utf_to_utf;
    return utf_to_utf<char>(str.c_str(), str.c_str() + str.size());
  }
#endif

  std::string to_lower(const std::string& src)
  {
    using namespace std;

    auto u32src = utf8_to_u32string(src);
    auto u32dst = u32string(u32src.size(), 0);

    transform(begin(u32src), end(u32src), back_inserter(u32dst),
              [](const char32_t c) {
                return utils::towlower(c);
              });

    return u32string_to_utf8(u32dst);
  }


  std::string to_upper(const std::string& src)
  {
    using namespace std;

    auto u32src = utf8_to_u32string(src);
    auto u32dst = u32string(u32src.size(), 0);

    transform(begin(u32src), end(u32src), back_inserter(u32dst),
              [](const char32_t c) {
                return utils::towupper(c);
              });

    return u32string_to_utf8(u32dst);
  }

} // ns utils
} // ns eyestep
