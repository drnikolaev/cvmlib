cmake_minimum_required(VERSION 3.18)
project(CVM LANGUAGES CXX Fortran)

option(ILP64 "Set default integer size to 8 byte" OFF)
option(ICC "Use Intel C and C++ compilers" OFF)
option(IFORT "Use Intel Fortran compiler" OFF)
option(MKL "Link with Intel MKL" ON)

if(ICC)
    SET(CMAKE_C_COMPILER icc)
    SET(CMAKE_CXX_COMPILER icpc)
endif()
if(IFORT)
    SET(CMAKE_Fortran_COMPILER ifort)
endif()

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/cvm)
