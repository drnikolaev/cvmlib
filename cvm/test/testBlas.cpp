//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2016
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "StdAfx.h"
#include "test.h"

template <typename T>
class BlasTest : public ::testing::Test {
protected:
    BlasTest() {}
    virtual ~BlasTest() {}
};

TYPED_TEST_CASE(BlasTest, TestTypes);

TYPED_TEST(BlasTest, TestNorm1) {
    TP a[] = {1., 2., 3., -4., 5., -6.};
    const basic_rvector<TP> vr(a, 6);
    const basic_cvector<TP,TPC> vc((TPC*) a, 3);
    EXPECT_EQ(TP(21.), vr.norm1()) << "rvector::norm1";
    EXPECT_NEAR(15.04631765340644,vc.norm1(),sf<TP>()) << "cvector::norm1";
}
