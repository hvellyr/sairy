# -*-meson-*-
# Copyright (c) 2020 Gregor Klinke
# All rights reserved.

project('Textbook', ['cpp', 'c'],
        version : '0.1',
        license : 'BSD',
        meson_version : '>= 0.54',
        default_options : ['cpp_std=c++14',
                           'buildtype=release'])

set_variable('copyright_year', '2017-2020')
set_variable('copyright_owner', 'Gregor C. Klinke')

if get_option('buildtype') == 'debug'
  add_project_arguments('-DIS_DEBUG=1', language: 'cpp')
endif

conf_data = configuration_data()
conf_data.set('version', meson.project_version())
conf_data.set('copyright_year', copyright_year)
conf_data.set('copyright_owner', copyright_owner)

prefix = get_option('prefix')
conf_data.set('prefix', prefix)
conf_data.set('bindir', prefix / get_option('bindir'))
conf_data.set('libdir', prefix / get_option('libdir'))
conf_data.set('pkglibdir', prefix / get_option('libdir') / 'textbook')
conf_data.set('includedir', prefix / get_option('includedir'))
conf_data.set('pkgincludedir', prefix / get_option('includedir') / 'textbook')
conf_data.set('datadir', prefix / get_option('datadir'))

pkgdatadir = prefix / get_option('datadir') / 'textbook'
conf_data.set('pkgdatadir', pkgdatadir)
conf_data.set('textbook_default_prefix', pkgdatadir)

if host_machine.system() == 'darwin'
  conf_data.set('OS_mac', 1)
elif host_machine.system() == 'windows'
  conf_data.set('OS_win', 1)
elif host_machine.system() == 'linux'
  conf_data.set('OS_linux', 1)
endif


# ------------------------------------------------------------------------------

cc = meson.get_compiler('cpp')


if cc.compiles('''#include <optional>

int main() {
  auto o = std::optional<int>{42};
  return *o;
}''',
               name : 'std::optional available')
  set_variable('supports_std_optional', true)
else
  set_variable('supports_std_optional', false)
endif


if cc.compiles('''#include <memory>

class Foo { public: int m = 42; };

int main() {
  auto o = std::make_unique<Foo>();
  return o->m == 42 ? 0 : 1;
}
''',
               name : 'std::make_unique available')
  set_variable('supports_std_make_unique', true)
else
  set_variable('supports_std_make_unique', false)
endif


if cc.compiles('''#include <codecvt>
#include <locale>

int main() {
  std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>, wchar_t> conv16;
  std::wstring dst = conv16.from_bytes(std::string("hello world"));

  return dst.size() != 0 ? 0 : 1;
}
''',
               name : 'std::codecvt available')
  set_variable('supports_std_codecvt', true)
else
  set_variable('supports_std_codecvt', false)
endif


if cc.compiles('''#include <type_traits>

template <typename T, typename U>
auto
foo(T t) -> std::enable_if_t<!std::is_array<T>::value, U>
{
  return static_cast<U>(t);
}

int main() {
  return 0;
}
''',
               name : 'std::enable_if_t available')
  set_variable('supports_std_enable_if_t', true)
else
  set_variable('supports_std_enable_if_t', false)
endif


if cc.compiles('''#include <sstream>

int main() {
  std::stringstream ss;
  ss << "foo";
  ss = std::stringstream("hello world", std::ios::out);
  return 0;
}
''',
               name : 'std::stringstream supports copy assignment')
  set_variable('supports_stringstream_copyassign', true)
else
  set_variable('supports_stringstream_copyassign', false)
endif


if cc.compiles('''#include <algorithm>
#include <vector>
int main() {
  auto v = std::vector<int>{1, 2, 3, 4};
  auto b = std::vector<int>{1, 2};
  return mismatch(begin(v), end(v), begin(b), end(b),
                  [](int a, int b) {
                    return a == b;
                  }).first == begin(v);
}
''',
               name : 'std::mismatch has a predicate')
  set_variable('supports_std_mismatch_with_pred', true)
else
  set_variable('supports_std_mismatch_with_pred', false)
endif


set_variable('supports_no_shift_negative_value_warning',
             cc.has_argument('-Wno-shift-negative-value'))

set_variable('supports_no_expansion_to_defined_warning',
             cc.has_argument('-Wno-expansion-to-defined'))

# ------------------------------------------------------------------------------

catch_inc = include_directories('third-party')
json_inc = include_directories('third-party/nlohmann_json')

fspp = subproject('fspplib')
fspp_dep = fspp.get_variable('fspp_dep')

subdir('third-party')
subdir('src')
subdir('share')



# ------------------------------------------------------------------------------
# create a starter script which can be used to run textbook uninstalled

tb_data = configuration_data()
tb_data.set('textbook_executable_path', textbook.full_path())
tb_data.set('project_source_dir', meson.current_source_dir())

tbsh = configure_file(input : 'bin/textbook.in',
                      output : 'textbook',
                      configuration : tb_data,
                      install : false)

starter_script = join_paths(meson.current_source_dir(), 'bin/textbook')
custom_target('textbook_starter',
              command : [import('python').find_installation('python3'),
                         '-c',
                         'import shutil; shutil.copy("@0@", "@1@")'.format(tbsh,
                                                                           starter_script)],
              depend_files : [tbsh],
              input : [tbsh],
              output : 'foo',
              build_by_default : true,
              build_always_stale : true,
              install : false,
              )
