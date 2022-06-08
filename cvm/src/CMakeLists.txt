cmake_minimum_required(VERSION 3.6)
#project(cvm)
set(CMAKE_CXX_STANDARD 11)
set(OUTPUT_PATH  "${CMAKE_CURRENT_SOURCE_DIR}/../../lib64")
link_directories(${OUTPUT_PATH} ${OUTPUT_PATH}/mac/gf /usr/local/lib)

FILE(GLOB SOURCE_FILES *.cpp *.h)
message("OUT" ${OUTPUT_PATH})
add_subdirectory(ftn)

if (NOT TARGET cvm)
    add_library(cvm SHARED ${SOURCE_FILES})

set_target_properties(
        cvm
        PROPERTIES
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        RUNTIME_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        LIBRARY_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
)

add_dependencies(cvm ftn)

if (CMAKE_BUILD_TYPE STREQUAL "Release")
    #    target_link_libraries(cvm optimized -Wl,--start-group lapack_em64t blas_em64t
    #            ${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES} -Wl,--end-group)
    target_link_libraries(cvm optimized lapack_em64t blas_em64t ftn_em64t gfortran)
    #            ${CMAKE_Fortran_LINK_LIBRARIES})
else()
    #    target_link_libraries(cvm debug lapack_ifort_em64t_debug blas_ifort_em64t_debug
    #            ${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES})
    target_link_libraries(cvm debug blas_em64t_debug lapack_em64t_debug ftn_em64t_debug gfortran)
    #           ${CMAKE_Fortran_LINK_LIBRARIES})
endif()

endif()
