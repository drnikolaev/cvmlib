cmake_minimum_required(VERSION 3.24.0)
project(ftn LANGUAGES Fortran)
file(GLOB F_SOURCES CONFIGURE_DEPENDS "*.f")
add_library(ftn STATIC ${F_SOURCES})
find_package(Threads REQUIRED)

set(_out "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib")
foreach(cfg Debug Release RelWithDebInfo MinSizeRel)
    string(TOUPPER "${cfg}" CFGU)
    set_target_properties(ftn PROPERTIES
            ARCHIVE_OUTPUT_DIRECTORY_${CFGU} "${_out}"
            LIBRARY_OUTPUT_DIRECTORY_${CFGU} "${_out}"
            RUNTIME_OUTPUT_DIRECTORY_${CFGU} "${_out}"
    )
endforeach()

set_target_properties(ftn PROPERTIES DEBUG_POSTFIX "_debug")

if(NOT WIN32)
    set_target_properties(ftn PROPERTIES POSITION_INDEPENDENT_CODE ON)
endif()

target_compile_options(ftn PRIVATE
        $<$<CONFIG:Release>:$<$<COMPILE_LANGUAGE:Fortran>:-O3>>
        $<$<CONFIG:Debug>:$<$<COMPILE_LANGUAGE:Fortran>:-O0 -g>>
)

if(UNIX AND NOT APPLE)
    target_compile_options(ftn PRIVATE -pthread)
    target_link_options(ftn PRIVATE -pthread)
endif()

if(ILP64)
    if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
        if(WIN32)
            target_compile_options(ftn PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:/integer-size:64>)
        else()
            target_compile_options(ftn PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-integer-size 64>)
        endif()
    elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        target_compile_options(ftn PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-fdefault-integer-8>)
    elseif(MSVC)
        # Intel Fortran with MSVC toolchain is covered above; MSVC itself is rare for Fortran.
    endif()
    # Optionally add a shared interface macro that C/C++ can also see
    target_compile_definitions(ftn PRIVATE USE_ILP64=1)
endif()
