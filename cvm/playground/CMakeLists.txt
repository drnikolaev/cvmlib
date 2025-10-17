cmake_minimum_required(VERSION 3.23.0)
project(playground LANGUAGES CXX VERSION 1 DESCRIPTION "CVM Class Library playground")
FILE(GLOB SOURCE_FILES *.cpp)
set(OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../lib")

add_executable(playground ${SOURCE_FILES})
#add_dependencies(playground cvm)

set_target_properties(
        playground
        PROPERTIES
        CXX_STANDARD 20
        CXX_EXTENSIONS OFF
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        RUNTIME_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        LIBRARY_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
)

if(ILP64)
    target_compile_definitions(playground PRIVATE CVM_ILP64)
endif()

if(LINUX OR MAC)
    target_compile_options(playground PRIVATE
            $<$<CONFIG:Release>:-O2 -lstdc++ >
            $<$<CONFIG:Debug>:-O0 -g -lstdc++ >)
else()
    target_compile_options(playground PRIVATE
            $<$<CONFIG:Release>:/MD /EHs /O1 /bigobj>
            $<$<CONFIG:Debug>:/MD /EHs /Od /debug:all /bigobj>)
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../cvm/src)

target_link_directories(playground PRIVATE ${OUTPUT_PATH})

target_link_libraries(playground optimized cvm stdc++)
target_link_libraries(playground debug cvm_debug stdc++)
