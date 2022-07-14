cmake_minimum_required(VERSION 3.18)
FILE(GLOB SOURCE_FILES *.cpp)
set(SOURCE_FILES ${SOURCE_FILES} test.h)
set(OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../lib")

add_executable(cvm_test ${SOURCE_FILES})
add_dependencies(cvm_test cvm)

set_target_properties(
        cvm_test
        PROPERTIES
        CXX_STANDARD 17
        CXX_EXTENSIONS OFF
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        RUNTIME_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        LIBRARY_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../gtest/googletest/include)
target_link_directories(cvm_test PUBLIC ${OUTPUT_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/../../gtest)

if (CMAKE_BUILD_TYPE STREQUAL "Release")
    target_link_libraries(cvm_test optimized cvm gtest)
else()
    target_link_libraries(cvm_test debug cvm_debug gtest)
endif()