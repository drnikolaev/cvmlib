cmake_minimum_required(VERSION 3.23.0)
project(cvm LANGUAGES Fortran CXX VERSION 9.2.0 DESCRIPTION "CVM Class Library")

#cmake -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_CXX_COMPILER=icx -DCMAKE_C_COMPILER=icx -DMKL=ON -DILP64=ON -DBUILD_TESTS=ON
#cmake -DCMAKE_Fortran_COMPILER=aarch64-linux-gnu-gfortran -DCMAKE_CXX_COMPILER=aarch64-linux-gnu-g++ -DCMAKE_C_COMPILER=aarch64-linux-gnu-gcc -DILP64=ON -DBUILD_TESTS=ON -S . -B build
#PATH=/opt/arm/gcc-14.2.0_Ubuntu-22.04/bin:$PATH cmake -DCMAKE_Fortran_COMPILER=aarch64-linux-gnu-gfortran -DCMAKE_CXX_COMPILER=aarch64-linux-gnu-g++  -DILP64=ON -DARMPL=ON -DBUILD_TESTS=ON  -B build/ -S .
#cmake -DCMAKE_Fortran_COMPILER=aarch64-apple-darwin24-gfortran-mp-14 -DCMAKE_CXX_COMPILER=aarch64-apple-darwin24-c++-mp-14  -DARMPL=ON -DBUILD_TESTS=ON  -B build/ -S .

if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64")
    SET(ARM ON)
    message("Building CVM Class Library for ARM64")
    if(ARMPL)
        find_library(ARMPL_LIB NAMES armpl_ilp64 PATHS /opt/arm/* PATH_SUFFIXES lib)
        find_library(ARMFL_LIB NAMES armflang PATHS /opt/arm/* PATH_SUFFIXES lib)
        get_filename_component(ARMPL_DIR "${ARMPL_LIB}" DIRECTORY)
        get_filename_component(ARMFL_DIR "${ARMFL_LIB}" DIRECTORY)
        message("ARMPL found: ${ARMPL_DIR}" )
        message("ARMFL found: ${ARMFL_DIR}" )
    endif ()
endif()

if (${CMAKE_CXX_COMPILER_ID} MATCHES "^Intel.*")
    SET(ICC ON)
    message(STATUS "Found Intel C++: ${CMAKE_CXX_COMPILER_ID} version ${CMAKE_CXX_COMPILER_VERSION}")
else()
    message(STATUS "Found C++: ${CMAKE_CXX_COMPILER_ID} version ${CMAKE_CXX_COMPILER_VERSION}")
endif()

if (${CMAKE_Fortran_COMPILER_ID} MATCHES "^Intel.*")
    SET(IFORT ON)
    message(STATUS "Found Intel Fortran: ${CMAKE_Fortran_COMPILER_ID} version ${CMAKE_Fortran_COMPILER_VERSION}")
else ()
    message(STATUS "Found Fortran: ${CMAKE_Fortran_COMPILER_ID} version ${CMAKE_Fortran_COMPILER_VERSION}")
endif()
if (ILP64)
    message(STATUS "ILP64 is ON: setting the default integer size to 8 byte")
endif ()

option(BUILD_TESTS "Build regression test program" OFF)
option(MKL "Use Intel MKL library" OFF)
option(ARMPL "Use ARM Performance Library" OFF)
option(ILP64 "Set the default integer size to 8 bytes" OFF)
option(CVM_USE_POOL_MANAGER "Use internal pool manager, for debugging only!" OFF)

if(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
    option(MAC "Target platform is Mac OS" ON)
else()
    option(MAC "Target platform is Mac OS" OFF)
endif()

add_subdirectory(cvm)
