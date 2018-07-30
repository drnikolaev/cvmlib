cmake_minimum_required(VERSION 3.6)
#project(Tools CXX Fortran)
#enable_language (Fortran)
set(CMAKE_CXX_STANDARD 11)
FILE(GLOB SOURCE_FILES *.cpp *.h)
set(OUTPUT_PATH  "${CMAKE_CURRENT_SOURCE_DIR}/../../lib64")

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../src ${CMAKE_CURRENT_SOURCE_DIR}/../../lib64)
link_directories(${OUTPUT_PATH} ${OUTPUT_PATH}/mac/gf /usr/local/lib ${CMAKE_CURRENT_SOURCE_DIR}/../../gtest)

message ("files " ${SOURCE_FILES})
add_executable(cvm_test ${SOURCE_FILES})
add_dependencies(cvm_test cvm)

set_target_properties(
        cvm_test
        PROPERTIES
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        RUNTIME_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        LIBRARY_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../gtest/googletest/include)

if (CMAKE_BUILD_TYPE STREQUAL "Release")
    target_link_libraries(cvm_test optimized cvm gtest)
else()
    target_link_libraries(cvm_test debug cvm_debug gtest)
endif()

