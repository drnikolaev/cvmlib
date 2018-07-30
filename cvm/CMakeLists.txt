cmake_minimum_required(VERSION 3.6)
project(Tools CXX Fortran)
enable_language (Fortran)
set(CMAKE_CXX_STANDARD 11)
add_subdirectory(src)
add_subdirectory(test)

