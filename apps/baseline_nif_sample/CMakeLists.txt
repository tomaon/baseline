cmake_minimum_required(VERSION 2.6)

project(baseline_nif_sample)

set(WITH_ERLANG ON)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
set(CMAKE_SHARED_MODULE_PREFIX_C "")

set(LIBS)

find_package(ErlInterface)

add_definitions("-D_REENTRANT")
add_definitions("-std=c99")
add_definitions("-fPIC")
add_definitions("-fno-common")
add_definitions("-Wall -Wextra")
add_definitions("-Wstrict-prototypes")
add_definitions("-Wno-missing-field-initializers")

if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
  set(CMAKE_SHARED_MODULE_CREATE_C_FLAGS
    "${CMAKE_SHARED_MODULE_CREATE_C_FLAGS} -flat_namespace -undefined suppress")
endif()

add_library(baseline_nif_sample MODULE
  c_src/baseline_nif.c
  c_src/baseline_nif_sample.c)

install(TARGETS baseline_nif_sample DESTINATION "${CMAKE_SOURCE_DIR}/priv")
