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

if(ILP64)
    target_compile_definitions(cvm_test PUBLIC CVM_ILP64)
endif()

if(MAC)
    target_compile_definitions(cvm_test PUBLIC CVM_ON_MAC)
endif()

if(LINUX OR MAC)
    target_compile_options(cvm_test PUBLIC
            $<$<CONFIG:Release>:-O2>
            $<$<CONFIG:Debug>:-O0 -g>)
else()
    target_compile_options(cvm_test PUBLIC /MT
            $<$<CONFIG:Release>:/O1>
            $<$<CONFIG:Debug>:/Od>)
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../gtest/googletest/include)
target_link_directories(cvm_test PUBLIC ${OUTPUT_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/../../gtest ${CMAKE_CURRENT_SOURCE_DIR}/../../gtest/lib)

target_link_libraries(cvm_test optimized cvm)
target_link_libraries(cvm_test debug cvm_debug)
if(LINUX OR MAC)
    target_link_libraries(cvm_test gtest pthread)
else()
    target_link_libraries(cvm_test gtest)
endif()
