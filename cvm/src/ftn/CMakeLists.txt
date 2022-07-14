cmake_minimum_required(VERSION 3.18)
project(ftn)
enable_language(Fortran)

#SET(CMAKE_Fortran_COMPILER_WORKS 1)
SET(CMAKE_Fortran_COMPILER /opt/intel/oneapi/compiler/latest/mac/bin/intel64/ifort)

#get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
#
#message(${Fortran_COMPILER_NAME})
#
#if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
#    # gfortran
#    set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
#    set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g")
#elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
    # ifort (untested)
#    set (CMAKE_Fortran_FLAGS_RELEASE "-O3")
#    set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
#elseif (Fortran_COMPILER_NAME MATCHES "g77")
#    # g77
#    set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
#    set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
#endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

FILE(GLOB SOURCE_FILES *.f)

if (NOT TARGET ftn)
    add_library(ftn STATIC ${SOURCE_FILES})
endif()

if(ILP64)
    add_compile_options(-i8)
endif()

add_compile_options(-threads
        $<$<CONFIG:Release>:-O3>
        $<$<CONFIG:Debug>:-O0 -g>)

set_target_properties(ftn
        PROPERTIES
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib")
