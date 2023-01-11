cmake_minimum_required(VERSION 3.23)
SET (CVM_VERSION 9.1.0)
project(cvm VERSION ${CVM_VERSION} DESCRIPTION "CVM Class Library")

option(BUILD_TESTS "Build regression test program" OFF)
option(IFORT "Use Intel Fortran compiler" OFF)
option(ICC "Use Intel C++ compiler" OFF)
option(MKL "Use Intel MKL library" OFF)
option(ILP64 "Set default integer size to 8 byte" OFF)

add_subdirectory(cvm)
