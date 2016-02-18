#!/bin/bash
git submodule update --init --recursive
pushd gtest
cmake .
make clean
make gtest
popd
cp gtest/googlemock/gtest/*.a gtest
