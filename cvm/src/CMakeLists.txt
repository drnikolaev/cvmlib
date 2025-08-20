set(CMAKE_CXX_STANDARD 20)
set(OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../lib")
find_package(Threads REQUIRED)

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

add_library(cvm SHARED ${SOURCE_FILES})

set_target_properties(
        cvm
        PROPERTIES
        VERSION ${PROJECT_VERSION}
        SOVERSION ${PROJECT_VERSION}
        PUBLIC_HEADER cvm.h
        CXX_STANDARD 20
        CXX_EXTENSIONS OFF
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        RUNTIME_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        LIBRARY_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        POSITION_INDEPENDENT_CODE ON
)
add_dependencies(cvm ftn)

if(CVM_USE_POOL_MANAGER)
    message("Using Pool Manager!")
    target_compile_definitions(cvm PRIVATE CVM_USE_POOL_MANAGER)
endif()
if(ILP64)
    target_compile_definitions(cvm PRIVATE CVM_ILP64)
endif()

target_include_directories(cvm PRIVATE .)
if(LINUX OR MAC)
    target_compile_options(cvm PRIVATE
        $<$<CONFIG:Release>:-O3>
        $<$<CONFIG:Debug>:-O0 -g>)
else()
    target_compile_definitions(cvm PRIVATE SRC_EXPORTS)
    target_compile_options(cvm PRIVATE
            $<$<CONFIG:Release>:/O2>
            $<$<CONFIG:Debug>:/Od>)
endif()

if(MAC AND NOT ARMPL)
    target_compile_definitions(cvm PRIVATE CVM_ON_MAC)
endif ()

target_link_directories(cvm PRIVATE ${OUTPUT_PATH} ${CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES} ${ARMPL_DIR} ${ARMFL_DIR})
#target_link_libraries(cvm optimized ftn)
#target_link_libraries(cvm debug ftn_debug)
# Let CMake resolve the correct config-specific library (DEBUG_POSTFIX covers file name)
target_link_libraries(cvm PRIVATE ftn)

if(LINUX)
    target_link_libraries(cvm Threads::Threads)
endif()

if(ARM)
    if(ARMPL)
        if(ILP64)
            if(MAC)
                target_link_libraries(cvm PRIVATE armpl_ilp64)
            else()
                target_link_libraries(cvm PRIVATE armflang armpl_ilp64)
            endif()
        else()
            if(MAC)
                target_link_libraries(cvm PRIVATE armpl_lp64)
            else()
                target_link_libraries(cvm PRIVATE armflang armpl_lp64)
            endif()
        endif()
    else()
        if(ILP64)
            target_link_libraries(cvm PRIVATE blas64 lapack64)
        else()
            target_link_libraries(cvm PRIVATE blas lapack)
        endif()
    endif()
else()
    if(MKL)
        if(ILP64)
            if(ICC AND NOT LINUX)
                target_compile_options(cvm PRIVATE -Qmkl-ilp64:parallel)
            endif()
            if(IFORT)
                target_link_libraries(cvm PRIVATE mkl_intel_ilp64)
            else()
                target_link_libraries(cvm PRIVATE mkl_gf_ilp64)
            endif()
        else()
            if(ICC AND NOT LINUX)
                target_compile_options(cvm PRIVATE -Qmkl=parallel)
            endif()
            if(IFORT)
                target_link_libraries(cvm PRIVATE mkl_intel_lp64)
            else()
                target_link_libraries(cvm PRIVATE mkl_gf_lp64)
            endif()
        endif()
        target_link_libraries(cvm PRIVATE mkl_core)
        if(ICC)
            target_link_libraries(cvm PRIVATE mkl_intel_thread)
        else()
            target_link_libraries(cvm PRIVATE mkl_gnu_thread omp5)
        endif()
    else()
        if(ILP64)
            if(MAC)
                target_compile_definitions(cvm PRIVATE ACCELERATE_NEW_LAPACK)
                target_compile_definitions(cvm PRIVATE ACCELERATE_LAPACK_ILP64)
                target_link_libraries(cvm blas lapack)
            else()
                target_link_libraries(cvm blas64 lapack64)
            endif()
        else()
            if(MAC)
                target_compile_definitions(cvm PRIVATE ACCELERATE_NEW_LAPACK)
                target_link_libraries(cvm blas lapack)
            else()
                target_link_libraries(cvm blas lapack)
            endif()
        endif()
    endif()
endif()

if(LINUX)
    if(ICC)
        target_link_libraries(cvm iomp5)
    endif()
endif()

if(MAC OR ARM)
    target_compile_definitions(cvm PRIVATE CVM_DOTU_WAR)
endif()
