#!/bin/bash
git submodule update --init --recursive
pushd gtest
git pull origin main
cmake . -DCMAKE_C_COMPILER_WORKS=1 -DCMAKE_CXX_COMPILER_WORKS=1
make clean
make gtest
popd
cp ./gtest/lib/libgtest.a gtest 2>/dev/null || :
#cp ./gtest/googlemock/gtest/libgtest.a gtest 2>/dev/null || :
