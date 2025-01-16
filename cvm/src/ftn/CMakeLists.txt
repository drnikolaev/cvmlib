FILE(GLOB SOURCE_FILES *.f)
add_library(ftn STATIC ${SOURCE_FILES})

set_target_properties(ftn
        PROPERTIES
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
        )

if(LINUX OR MAC)
    target_compile_options(ftn PUBLIC -pthread -fPIC
        $<$<CONFIG:Release>:-O3>
        $<$<CONFIG:Debug>:-O0 -g>)
else()
    target_compile_options(ftn PUBLIC
            $<$<CONFIG:Release>:/O2>
            $<$<CONFIG:Debug>:/Od /debug>)
endif()


if(LINUX)
    if(ILP64)
        if(IFORT)
            target_compile_options(ftn PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-integer-size 64>)
        else()
            target_compile_options(ftn PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-fdefault-integer-8>)
        endif()
    endif()
else()
    if(ILP64)
        if(IFORT)
            target_compile_options(ftn PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:/integer-size:64>)
        else()
            if(MAC)
                target_compile_options(ftn PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-fdefault-integer-8>)
            else()
                target_compile_options(ftn PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:/default-integer-8>)
            endif()
        endif()
    endif()
endif()

