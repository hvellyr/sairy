Textbook is an experimental documentation preparation facility.  It will use a
pluggable and extensible architecture.


Requirements
------------

  - cmake >= 2.8.12
  - a decent c++11 enabled compiler, apple's clang "6.0" should do

Development is happening mostly on Mac OS X for now.


Build and Test
--------------

  git submodule update --init --recursive
  mkdir build
  cd build
  cmake ..
  cmake --build .

The unit tests can be executed with ctest:

  ctest -V
