FILE(GLOB SOURCE_FILES *.cpp)
set(SOURCE_FILES ${SOURCE_FILES} test.h)
set(OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../lib")

if(ICC)
    SET(CMAKE_CXX_COMPILER /opt/intel/oneapi/compiler/latest/linux/bin/icpx)
endif()

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

if(ILP64)
    target_compile_definitions(cvm_test PUBLIC CVM_ILP64)
endif()
if(ICC)
    target_compile_options(cvm_test PUBLIC -fsycl -Rdebug-disables-optimization)
endif()
target_compile_options(cvm_test PUBLIC
        $<$<CONFIG:Release>:-O2>
        $<$<CONFIG:Debug>:-O0 -g>)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../gtest/googletest/include)
target_link_directories(cvm_test PUBLIC ${OUTPUT_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/../../gtest)

target_link_libraries(cvm_test optimized cvm)
target_link_libraries(cvm_test debug cvm_debug)
target_link_libraries(cvm_test gtest pthread)
