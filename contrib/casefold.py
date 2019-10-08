# Copyright (c) 2017 Gregor Klinke
# All rights reserved.

import sys
import re

RE = re.compile('^([0-9A-F]+); C; ([0-9A-F]+); .*CAPITAL.*')

lowertbl = {}
uppertbl = {}

for line in sys.stdin:
    m = RE.search(line)
    if m:
        lowertbl[m.group(1)] = m.group(2)
        uppertbl[m.group(2)] = m.group(1)


print r'''// DON'T EDIT THIS FILE
// it has been automatically create by casefold.py

#include <unordered_map>

namespace eyestep {
namespace utils {
namespace {
std::unordered_map<char32_t, char32_t> lower_to_upper_tbl = {'''

for k in lowertbl:
    print '  {{ 0x{}, 0x{} }},'.format(lowertbl[k], k)

print r'''};


std::unordered_map<char32_t, char32_t> upper_to_lower_tbl = {'''

for k in uppertbl:
    print '  {{ 0x{}, 0x{} }},'.format(uppertbl[k], k)

print r'''};
} // namespace

char32_t towupper(char32_t c) {
  auto i = lower_to_upper_tbl.find(c);
  return i != lower_to_upper_tbl.end()
    ? i->second
    : c;
}

char32_t towlower(char32_t c) {
  auto i = upper_to_lower_tbl.find(c);
  return i != upper_to_lower_tbl.end()
    ? i->second
    : c;
}

} // namespace utils
} // namespace eyestep'''
