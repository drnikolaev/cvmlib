cmake_minimum_required(VERSION 3.18)
project(ftn LANGUAGES Fortran)
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message("Discovered Fortran compiler: " ${Fortran_COMPILER_NAME})

if (CMAKE_BUILD_TYPE STREQUAL "Release")
    add_compile_options(-O3)
else()
    add_compile_options(-O0 -g)
endif()

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
    add_compile_options(-pthread)
    if(ILP64)
        add_compile_options(-fdefault-integer-8)
    endif()
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
    add_compile_options(-threads)
    if(ILP64)
        add_compile_options(-i8)
    endif()
elseif (Fortran_COMPILER_NAME MATCHES "f95.*")
    add_compile_options(-pthread)
    if(ILP64)
        add_compile_options(-fdefault-integer-8)
    endif()
endif ()

FILE(GLOB SOURCE_FILES *.f)
add_library(ftn STATIC ${SOURCE_FILES})

set_target_properties(ftn
        PROPERTIES
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib")
