cmake_minimum_required(VERSION 3.26.0)
project(cvm LANGUAGES Fortran CXX VERSION 9.1.0 DESCRIPTION "CVM Class Library")

#cmake -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_CXX_COMPILER=icx -DCMAKE_C_COMPILER=icx -DMKL=ON -DILP64=ON -DBUILD_TESTS=ON

if (${CMAKE_CXX_COMPILER_ID} MATCHES "^Intel.*")
    SET(ICC ON)
    message(STATUS "Found Intel C++: ${CMAKE_CXX_COMPILER_ID} version ${CMAKE_CXX_COMPILER_VERSION}")
endif()

if (${CMAKE_Fortran_COMPILER_ID} MATCHES "^Intel.*")
    SET(IFORT ON)
    message(STATUS "Found Intel Fortran: ${CMAKE_Fortran_COMPILER_ID} version ${CMAKE_Fortran_COMPILER_VERSION}")
else ()
    message(STATUS "Found Fortran: ${CMAKE_Fortran_COMPILER_ID} version ${CMAKE_Fortran_COMPILER_VERSION}")
endif()

option(BUILD_TESTS "Build regression test program (takes a while)" OFF)
option(MAC "Target platform is Mac OS" OFF)
option(MKL "Use Intel MKL library" OFF)
option(ILP64 "Set the default integer size to 8 byte" OFF)

add_subdirectory(cvm)
