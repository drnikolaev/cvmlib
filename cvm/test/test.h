//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2022
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#if defined(_MSC_VER)
#   pragma warning(push)
#   pragma warning(disable:4018)
#endif

#include <gtest/gtest.h>
#include <algorithm>

#if defined(_MSC_VER)
#   pragma warning(pop)
#   pragma warning(disable:4305)
#endif

#include "../src/cvm.h"
#include "../src/cfun.h"

using namespace cvm;

typedef ::testing::Types<float, double> TestTypes;

#define TP TypeParam
#define TPC std::complex<TypeParam>

template<typename T>
constexpr inline T s() {
    return sizeof(T) >= 8 ? T(1.e-14) : T(1.e-7);
}

template<typename T>
constexpr inline T sf() {
    return sizeof(T) >= 8 ? T(1.e-13) : T(1.e-5);
}

template<typename T>
constexpr inline T sp() {
    return sizeof(T) >= 8 ? T(1.e-10) : T(5.e-4);
}

template<typename T>
constexpr inline T spp() {
    return sizeof(T) >= 8 ? T(1.e-5) : T(5.e-3);
}

template<typename T>
constexpr inline T spp(double fine, double coarse) {
    return sizeof(T) >= 8 ? T(fine) : T(coarse);
}

