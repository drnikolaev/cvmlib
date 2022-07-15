cmake_minimum_required(VERSION 3.18)
FILE(GLOB SOURCE_FILES *.cpp)
set(SOURCE_FILES ${SOURCE_FILES} test.h)
set(OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../lib")

add_executable(cvm_test ${SOURCE_FILES})

set_target_properties(
        cvm_test
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

target_include_directories(cvm_test PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/../../gtest/googletest/include)
target_link_directories(cvm_test PUBLIC ${OUTPUT_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/../../gtest)

add_dependencies(cvm_test cvm)
if (CMAKE_BUILD_TYPE STREQUAL "Release")
    target_link_libraries(cvm_test cvm gtest pthread)
else()
    target_link_libraries(cvm_test cvm_debug gtest pthread)
endif()
