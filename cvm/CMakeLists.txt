cmake_minimum_required(VERSION 3.6)
#set(CMAKE_TRY_COMPILE_TARGET_TYPE "STATIC_LIBRARY")
#SET (CMAKE_C_COMPILER_WORKS 1)
#SET (CMAKE_CXX_COMPILER_WORKS 1)


project(CVM LANGUAGES CXX Fortran)
#enable_language (Fortran)
set(CMAKE_CXX_STANDARD 11)
add_subdirectory(src)
add_subdirectory(test)

