FILE(GLOB SOURCE_FILES *.cpp)
set(SOURCE_FILES ${SOURCE_FILES} test.h)
set(OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../lib")

add_executable(cvm_test ${SOURCE_FILES})
add_dependencies(cvm_test cvm)

set_target_properties(
        cvm_test
        PROPERTIES
        CXX_STANDARD 20
        CXX_EXTENSIONS OFF
        DEBUG_POSTFIX _debug
        ARCHIVE_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        RUNTIME_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
        LIBRARY_OUTPUT_DIRECTORY "${OUTPUT_PATH}"
)

if(CVM_USE_POOL_MANAGER)
    message("Tests using Pool Manager!")
    target_compile_definitions(cvm_test PUBLIC CVM_USE_POOL_MANAGER)
endif()
if(ILP64)
    target_compile_definitions(cvm_test PUBLIC CVM_ILP64)
endif()

if(MAC OR ARM)
    target_compile_definitions(cvm_test PUBLIC CVM_DOTU_WAR)
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
target_link_directories(cvm_test PUBLIC ${OUTPUT_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/../../gtest ${CMAKE_CURRENT_SOURCE_DIR}/../../gtest/lib ${ARMPL_DIR} ${ARMFL_DIR})

target_link_libraries(cvm_test optimized cvm)
target_link_libraries(cvm_test debug cvm_debug)
if(LINUX OR MAC)
    target_link_libraries(cvm_test gtest pthread)
else()
    target_link_libraries(cvm_test gtest)
endif()

include(CTest)
include(FetchContent)
# Configure googletest fetch
#    set(BUILD_GMOCK OFF CACHE BOOL "" FORCE)
#    set(BUILD_GTEST ON CACHE BOOL "" FORCE)
#    set(INSTALL_GTEST ON CACHE BOOL "" FORCE)

FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/refs/tags/v1.15.2.zip
        # Optional but recommended integrity check (update if you change the URL)
        URL_HASH SHA256=f179ec217f9b3b3f3c6e8b02d3e7eda997b49e4ce26d6b235c9053bec9c0bf9f
)
FetchContent_MakeAvailable(googletest)

# 4) Test target(s)
#    add_executable(hello_gtest tests/test1.cpp)
#target_link_libraries(cvm_test PRIVATE fastcauchy_lib GTest::gtest_main)
target_include_directories(cvm_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

include(GoogleTest)
gtest_discover_tests(cvm_test)
