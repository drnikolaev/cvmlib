cmake_minimum_required(VERSION 3.18)

SET (CMAKE_C_COMPILER_WORKS 1)
SET (CMAKE_CXX_COMPILER_WORKS 1)
SET (CMAKE_Fortran_COMPILER_WORKS 1)

#option(BUILD_TESTS "Build regression test program" ON)
option(ILP64 "Set default integer size to 8 byte" ON)

#set(if_cxx     $<COMPILE_LANGUAGE:CXX>)
#set(if_fortran $<COMPILE_LANGUAGE:Fortran>)
#set(cxx_flags
#        -Wno-deprecated-declarations
#        -Wall
#        $<$<CONFIG:Release>:-O3>
#        $<$<CONFIG:Debug>:-g>
#        )
#set(fortran_flags -assume nounderscore)
#
#add_compile_options(
#        "$<${if_cxx}:${cxx_flags}>"
#        "$<${if_fortran}:${fortran_flags}>"
#)




SET(CMAKE_C_COMPILER /opt/intel/oneapi/compiler/latest/mac/bin/intel64/icc)
SET(CMAKE_CXX_COMPILER /opt/intel/oneapi/compiler/latest/mac/bin/intel64/icpc)

project(CVM LANGUAGES CXX Fortran)
set(CMAKE_CXX_STANDARD 17)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/cvm)
