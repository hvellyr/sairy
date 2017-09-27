# Copyright (c) 2017 Gregor Klinke
# All rights reserved.

include(ExternalProject)

ExternalProject_Add(fspp
        GIT_SUBMODULES "third-party/fspplib"
        SOURCE_DIR "${PROJECT_SOURCE_DIR}/third-party/fspplib"
	INSTALL_DIR ${PROJECT_BINARY_DIR}
	CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
        STEP_TARGETS build install)
