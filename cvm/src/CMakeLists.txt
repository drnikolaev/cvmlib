cmake_minimum_required(VERSION 3.18)
#project(cvm)
SET (CMAKE_C_COMPILER_WORKS 1)
SET (CMAKE_CXX_COMPILER_WORKS 1)

SET(CMAKE_C_COMPILER /opt/intel/oneapi/compiler/latest/mac/bin/intel64/icc)
SET(CMAKE_CXX_COMPILER /opt/intel/oneapi/compiler/latest/mac/bin/intel64/icpc)

#set(CMAKE_CXX_STANDARD 17)
set(OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../lib")
#link_directories(${OUTPUT_PATH} ${OUTPUT_PATH}/intel64 /usr/local/lib)

FILE(GLOB SOURCE_FILES *.cpp *.h)
#message("OUT" ${OUTPUT_PATH})
add_subdirectory(ftn)

if (NOT TARGET cvm)
    add_library(cvm SHARED ${SOURCE_FILES})

set_target_properties(
        cvm
        PROPERTIES
        CXX_STANDARD 17
        CXX_EXTENSIONS OFF
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        RUNTIME_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        LIBRARY_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
)
add_dependencies(cvm ftn)

if(ILP64)
    add_compile_options(-fdefault-integer-8)
endif()

target_link_directories(cvm PUBLIC ${OUTPUT_PATH} /opt/intel/oneapi/mkl/latest/lib)
if (CMAKE_BUILD_TYPE STREQUAL "Release")
    if(ILP64)
        target_link_libraries(cvm optimized ftn mkl_intel_ilp64 mkl_intel_thread mkl_core iomp5 pthread m dl)
    else()
        target_link_libraries(cvm optimized ftn mkl_intel_lp64 mkl_intel_thread mkl_core iomp5 pthread m dl)
    endif()
else()
    if(ILP64)
        target_link_libraries(cvm debug ftn_debug mkl_intel_ilp64 mkl_intel_thread mkl_core iomp5 pthread m dl)
    else()
        target_link_libraries(cvm debug ftn_debug mkl_intel_lp64 mkl_intel_thread mkl_core iomp5 pthread m dl)
    endif()
endif()

endif()


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

#target_compile_options(MyLib PUBLIC $<$<COMPILE_LANGUAGE:CXX>:-std=c++14>)

#target_compile_features(MyLib PUBLIC cxx_explicit_conversions)

#add_executable(myapp main.cpp foo.c bar.cpp zot.cu)
#target_compile_definitions(myapp
#        PRIVATE $<$<COMPILE_LANG_AND_ID:CXX,AppleClang,Clang>:COMPILING_CXX_WITH_CLANG>
#        $<$<COMPILE_LANG_AND_ID:CXX,Intel>:COMPILING_CXX_WITH_INTEL>
#        $<$<COMPILE_LANG_AND_ID:C,Clang>:COMPILING_C_WITH_CLANG>
#        )


#cmake -D COMMIT_VERSION='"whatever version here"' ...
#add_definitions(-DCOMMIT_VERSION=${COMMIT_VERSION})


#project(P4V)
#cmake_minimum_required(VERSION 2.6)
#
#option(BUILD_STATIC_LIBS "Build the static library" ON)
#option(BUILD_SHARED_LIBS "Build the shared library" ON)
#option(BUILD_TESTS "Build test programs" OFF)
#
#include_directories(${CMAKE_SOURCE_DIR}/include)
#set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
#set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
#set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
#set(CMAKE_BUILD_TYPE Release)
#
#add_subdirectory(src)
#if(BUILD_TESTS)
#    add_subdirectory(tests)
#endif(BUILD_TESTS)
#
#Delete the CMakeCache.txt file and try this:
#
#cmake -G %1 -DBUILD_SHARED_LIBS=ON -DBUILD_STATIC_LIBS=ON -DBUILD_TESTS=ON ..
