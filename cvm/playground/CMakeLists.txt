cmake_minimum_required(VERSION 3.23.0)
project(playground LANGUAGES CXX VERSION 1 DESCRIPTION "CVM Class Library playground")
FILE(GLOB SOURCE_FILES *.cpp)
set(OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../lib")

add_executable(playground ${SOURCE_FILES})
#add_dependencies(playground cvm)

set_target_properties(
        playground
        PROPERTIES
        CXX_STANDARD 17
        CXX_EXTENSIONS OFF
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        RUNTIME_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        LIBRARY_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
)

if(ILP64)
    target_compile_definitions(playground PUBLIC CVM_ILP64)
endif()

if(LINUX)
    target_compile_options(playground PUBLIC
            $<$<CONFIG:Release>:-O2 -lstdc++ >
            $<$<CONFIG:Debug>:-O0 -g -lstdc++ >)
else()
    target_compile_options(playground PUBLIC /MT
            $<$<CONFIG:Release>:/O1>
            $<$<CONFIG:Debug>:/Od /debug:all>)
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../cvm/src)

target_link_directories(playground PUBLIC ${OUTPUT_PATH})

target_link_libraries(playground optimized cvm stdc++)
target_link_libraries(playground debug cvm_debug stdc++)
