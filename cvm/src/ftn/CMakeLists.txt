if(IFORT)
    SET(CMAKE_Fortran_COMPILER ifx)
endif()
project(ftn LANGUAGES Fortran)

FILE(GLOB SOURCE_FILES *.f)
add_library(ftn STATIC ${SOURCE_FILES})

set_target_properties(ftn
        PROPERTIES
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
        )

target_compile_options(ftn PUBLIC -pthread -fPIC
        $<$<CONFIG:Release>:-O3>
        $<$<CONFIG:Debug>:-O0 -g>)

if(ILP64)
    if(IFORT)
        target_compile_options(ftn PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-integer-size 64>)
    else()
        target_compile_options(ftn PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-fdefault-integer-8>)
    endif()
endif()
