cmake_minimum_required(VERSION 3.18)
set(OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../lib")
FILE(GLOB SOURCE_FILES *.cpp *.h)
add_subdirectory(ftn)
add_library(cvm SHARED ${SOURCE_FILES})
set_target_properties(
        cvm
        PROPERTIES
        CXX_STANDARD 17
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        RUNTIME_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        LIBRARY_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
)

if(ILP64)
    message("Using ILP64")
    add_definitions(-DCVM_ILP64=1)
endif()

add_dependencies(cvm ftn)
if (CMAKE_BUILD_TYPE STREQUAL "Release")
    target_link_libraries(cvm ftn)
else()
    target_link_libraries(cvm ftn_debug)
endif()

if(MKL)
    message("Using MKL")
    target_link_directories(cvm PUBLIC ${OUTPUT_PATH}) # /opt/intel/oneapi/mkl/latest/lib)
    if(ILP64)
        if(IFORT)
            target_link_libraries(cvm mkl_intel_ilp64)
        else()
            target_link_libraries(cvm mkl_gf_ilp64)
        endif()
    else()
        if(IFORT)
            target_link_libraries(cvm mkl_intel_lp64)
        else()
            target_link_libraries(cvm mkl_gf_lp64)
        endif()
    endif()
    target_link_libraries(cvm mkl_core)
    if(IFORT)
        target_link_libraries(cvm mkl_intel_thread)
    else()
        target_link_libraries(cvm mkl_gnu_thread)
    endif()
    target_link_libraries(cvm iomp5 pthread m dl)
else()
    message("Using LAPACK")
    target_link_libraries(cvm pthread m dl)
    if(ILP64)
        target_link_libraries(cvm lapack64 blas64)
    else()
        target_link_libraries(cvm lapack blas)
    endif()
endif()
