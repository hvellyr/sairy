Textbook is an experimental documentation preparation and extraction facility.
It will use a pluggable and extensible architecture.

Requirements
------------

  - cmake >= 2.8.12
  - boost >= 1.54
  - libclang 3.6.0
  - a decent c++11 enabled compiler, apple's clang "6.0" should do

Development is happening mostly on Mac OS X for now.


Build and Test
--------------

  mkdir build
  cd build
  cmake ..
  cmake --build .

The unit tests can be executed with ctest:

  ctest -V


You might set the following variables:

- Boost_INCLUDE_DIR   to the folder, where Boost headers are found
- LibClang_ROOT_DIR   to the folder, where LibClang headers and libraries are
                      installed
Example:

  cmake .. -DBoost_INCLUDE_DIR=/opt/local/include/ \
           -DLibClang_ROOT_DIR=/opt/local
