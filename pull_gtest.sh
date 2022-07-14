#!/bin/bash
git submodule update --init --recursive
pushd gtest
cmake . -DCMAKE_C_COMPILER_WORKS=1 -DCMAKE_CXX_COMPILER_WORKS=1
make clean
make gtest
popd
cp gtest/googlemock/gtest/*.a gtest
