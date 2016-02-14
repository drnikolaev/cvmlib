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
class InitializationTest : public ::testing::Test {
protected:
    InitializationTest() {}
    virtual ~InitializationTest() {}

    iarray ia;
    basic_rvector<T> rv;
    basic_rmatrix<T> rm;
    basic_srmatrix<T> srm;
    basic_srbmatrix<T> srbm;
    basic_srsmatrix<T> srsm;
    basic_cvector<T,TC> cv;
    basic_cmatrix<T,TC> cm;
    basic_scmatrix<T,TC> scm;
    basic_scbmatrix<T,TC> scbm;
    basic_schmatrix<T,TC> schm;

    rvector32 rv32;
    rmatrix32 rm32;
    srmatrix32 srm32;
    cvector32 cv32;
    cmatrix32 cm32;
    scmatrix32 scm32;
    srbmatrix32 srbm32;
    scbmatrix32 scbm32;
    srsmatrix32 srsm32;
    schmatrix32 schm32;

    rvector rv64;
    rmatrix rm64;
    srmatrix srm64;
    cvector cv64;
    cmatrix cm64;
    scmatrix scm64;
    srbmatrix srbm64;
    scbmatrix scbm64;
    srsmatrix srsm64;
    schmatrix schm64;
};

TYPED_TEST_CASE(InitializationTest, TestTypes);

TYPED_TEST(InitializationTest, TestSizes) {
    EXPECT_GE(sizeof(TP), 4);
    EXPECT_LE(sizeof(TP), 8);
    EXPECT_EQ(ia.size(), 0);
    EXPECT_EQ(rv.size(), 0);
    EXPECT_EQ(rm.size(), 0);
    EXPECT_EQ(rm.msize(), 0);
    EXPECT_EQ(rm.nsize(), 0);
    EXPECT_EQ(srm.size(), 0);
    EXPECT_EQ(srm.msize(), 0);
    EXPECT_EQ(srm.nsize(), 0);
    EXPECT_EQ(srbm.size(), 0);
    EXPECT_EQ(srbm.msize(), 0);
    EXPECT_EQ(srbm.nsize(), 0);
    EXPECT_EQ(srsm.size(), 0);
    EXPECT_EQ(srsm.msize(), 0);
    EXPECT_EQ(srsm.nsize(), 0);
    EXPECT_EQ(cv.size(), 0);
    EXPECT_EQ(cm.size(), 0);
    EXPECT_EQ(cm.msize(), 0);
    EXPECT_EQ(cm.nsize(), 0);
    EXPECT_EQ(scm.size(), 0);
    EXPECT_EQ(scm.msize(), 0);
    EXPECT_EQ(scm.nsize(), 0);
    EXPECT_EQ(scbm.size(), 0);
    EXPECT_EQ(scbm.msize(), 0);
    EXPECT_EQ(scbm.nsize(), 0);
    EXPECT_EQ(schm.size(), 0);
    EXPECT_EQ(schm.msize(), 0);
    EXPECT_EQ(schm.nsize(), 0);
}

#if defined(CVM_USE_INITIALIZER_LISTS)
TYPED_TEST(InitializationTest, TestInitList) {
    basic_rvector<TP> rv = { 1., -2., TP(3.456), TP(99.99) };
    basic_rvector<TP> rv0 = {};
    basic_cvector<TP,TPC> cv = { TPC(1.2, 3.4), TPC(3.4, 5.6), TP(99.99) };
    basic_rvector<TP> cv0 = {};
    EXPECT_EQ(rv0.size(), 0);
    EXPECT_EQ(cv0.size(), 0);
    EXPECT_EQ(rv[CVM0], 1.);
    EXPECT_NEAR(rv(CVM0 + 2), 3.456, sp<TP>());
    EXPECT_EQ(cv(CVM0), TPC(1.2, 3.4));
    EXPECT_EQ(cv(CVM0 + 2), TPC(99.99, 0.));
}
#endif

#if defined(CVM_USE_USER_LITERALS)
TYPED_TEST(InitializationTest, TestLiterals) {
    TPC c = 3.4 + 5.6_i;
    EXPECT_EQ(c, TPC(3.4, 5.6));

    const basic_cvector<TP,TPC> vc = { 2_i, -2_i, 2.1_i, -2.1_i,
        1 + 2_i, 1.1 + 2_i, 1 + 2.1_i, 1.1 + 2.1_i,
        2_i + 4, 2_i + 4.1, 2.1_i + 4, 2.1_i + 4.1,
        1 - 2_i, 1.1 - 2_i, 1 - 2.1_i, 1.1 - 2.1_i,
        2_i - 4, 2_i - 4.1, 2.1_i - 4, 2.1_i - 4.1 };
    EXPECT_NEAR(vc.norm(), 1.4972641717479251e+01, sp<TP>());
}
#endif
