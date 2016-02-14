//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2014
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#if defined(_MSC_VER)
#   pragma warning(push)
#   pragma warning(disable:4018)
#endif

#include <gtest/gtest.h>

#if defined(_MSC_VER)
#   pragma warning(pop)
#   pragma warning(disable:4305)
#endif

#include "../src/cvm.h"
#include "../src/cfun.h"

using namespace cvm;

typedef ::testing::Types<float, double> TestTypes;

#define TC std::complex<T>
#define TP TypeParam
#define TPC std::complex<TypeParam>

template<typename T>
inline T sp() {
    return sizeof(T) >= 8 ? T(1.e-10) : T(5.e-4);
}
