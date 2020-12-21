[![Build Status](https://travis-ci.org/hvellyr/textbook.svg?branch=master)](https://travis-ci.org/hvellyr/textbook)

Textbook is an experimental documentation preparation facility.  It will use a
pluggable and extensible architecture.


Requirements
------------

- meson >= 0.54
- a decent c++14 enabled compiler, apple's clang "6.0" should do

Development is happening mostly on Mac OS X for now.


Build and Test
--------------

```
$ git submodule update --init --recursive
$ meson setup --prefix=<PREFIX> output
$ cd output
$ meson compile
$ meson install
```

Some unit tests can be executed from within the build folder (`output`) with:

```
$ meson test
```
