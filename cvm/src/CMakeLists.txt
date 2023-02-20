set(CMAKE_CXX_STANDARD 17)
set(OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../lib")

add_subdirectory(ftn)

SET(SOURCE_FILES
        arrays.cpp
        cmatrix.cpp
        cvector.cpp
        globals.cpp
        rmatrix.cpp
        rvector.cpp
        scmatrix.cpp
        srmatrix.cpp
        utils.cpp
        )
if (NOT LINUX)
    SET(SOURCE_FILES  ${SOURCE_FILES}
            ../../cvmlib/framework.h
            ../../cvmlib/dllmain.cpp
            ../../cvmlib/pch.cpp
            )
endif()

add_library(cvm SHARED ${SOURCE_FILES})

set_target_properties(
        cvm
        PROPERTIES
        VERSION ${CVM_VERSION}
        SOVERSION ${CVM_VERSION}
        PUBLIC_HEADER cvm.h
        CXX_STANDARD 17
        CXX_EXTENSIONS OFF
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        RUNTIME_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        LIBRARY_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
)
add_dependencies(cvm ftn)

if(ILP64)
    target_compile_definitions(cvm PUBLIC CVM_ILP64)
endif()

target_include_directories(cvm PRIVATE .)
if(LINUX)
    target_compile_options(cvm PUBLIC -fPIC
        $<$<CONFIG:Release>:-O3>
        $<$<CONFIG:Debug>:-O0 -g>)
else()
    target_compile_definitions(cvm PUBLIC SRC_EXPORTS)
    target_compile_options(cvm PUBLIC
            $<$<CONFIG:Release>:/O3>
            $<$<CONFIG:Debug>:/Od /debug:all>)
endif()

target_link_directories(cvm PUBLIC ${OUTPUT_PATH})
target_link_libraries(cvm optimized ftn)
target_link_libraries(cvm debug ftn_debug)
if(LINUX)
    target_link_libraries(cvm pthread m dl)
endif()


if(MKL)
    if(ILP64)
        if(ICC AND NOT LINUX)
            target_compile_options(cvm PUBLIC -Qmkl-ilp64:parallel)
        endif()
        if(IFORT)
            target_link_libraries(cvm mkl_intel_ilp64)
        else()
            target_link_libraries(cvm mkl_gf_ilp64)
        endif()
    else()
        if(ICC AND NOT LINUX)
            target_compile_options(cvm PUBLIC -Qmkl=parallel)
        endif()
        if(IFORT)
            target_link_libraries(cvm mkl_intel_lp64)
        else()
            target_link_libraries(cvm mkl_gf_lp64)
        endif()
    endif()
    target_link_libraries(cvm mkl_core)
    if(ICC)
        target_link_libraries(cvm mkl_intel_thread)
    else()
        target_link_libraries(cvm mkl_gnu_thread omp5)
    endif()
else()
    if(ILP64)
        target_link_libraries(cvm blas64 lapack64)
    else()
        target_link_libraries(cvm blas lapack)
    endif()
endif()

if(LINUX)
    if(ICC)
        target_link_libraries(cvm iomp5)
    endif()
endif()
