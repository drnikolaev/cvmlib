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
class MiscTest : public ::testing::Test {
protected:
    MiscTest() {}
    virtual ~MiscTest() {}
};

TYPED_TEST_CASE(MiscTest, TestTypes);

TYPED_TEST(MiscTest, TestPrintProxy) {
    basic_rmatrix<TP> m(1,1);
    m(CVM0,CVM0) = 1.234;
    char buf[32];
#if defined (_MSC_VER) && !defined(__INTEL_COMPILER)
    sprintf_s (buf, sizeof(buf), "%.3f", m(CVM0,CVM0).val());  // use .val to printf type_proxy!
#else
    sprintf (buf, "%.3f", m(CVM0,CVM0).val());  // use .val to printf type_proxy!
#endif
    EXPECT_STREQ("1.234", buf);
}

TYPED_TEST(MiscTest, TestLUCrash) {
    basic_scbmatrix<TP,TPC> ma(3,1,0);
    ma.randomize_real(-1., 1.);
    ma.randomize_imag(-1., 1.);
    basic_scbmatrix<TP,TPC> mLU(3,1,0);
    tint nPivots[3];
    mLU.low_up(ma, nPivots);
}

TYPED_TEST(MiscTest, TestZeroResize) {
    TP r[16] = {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.};
    basic_rmatrix<TP> m(r,4,4);
    m.resize(3,0);
    EXPECT_EQ(0, m.size());
}
