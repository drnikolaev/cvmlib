//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2016
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "StdAfx.h"
#include "test.h"
//#include <iostream>

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

TYPED_TEST(MiscTest, TestMKL81Crash) {
    int i, j;
    const int n = 1000;
    const int p = 100;

    basic_rmatrix<TP> A(n, p);
    for (j = 0 ; j < p; ++j) {
       for (i = 0 ; i < n; ++i) {
           A(i+CVM0, j+CVM0) = TP(i + j * p);
       }
    }
    basic_rvector<TP> v(_cvm_min(n,p)) ;
    basic_srmatrix<TP> mU(n);
    basic_srmatrix<TP> mVH(p);

    v.svd(A, mU, mVH);
    basic_rmatrix<TP> mv(n,p);
    mv.diag(0) = v;

    EXPECT_NEAR(0., (A * ~mVH - mU * mv).norm(), spp<TP>(1.e-7,2.0));
    EXPECT_NEAR(0., (~A * mU - ~(mv * mVH)).norm(), spp<TP>(1.e-7,2.0));
}

TYPED_TEST(MiscTest, TestZeroResize) {
    TP r[16] = {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.};
    basic_rmatrix<TP> m(r,4,4);
    m.resize(3,0);
    EXPECT_EQ(0, m.size());
}

TYPED_TEST(MiscTest, TestMatrixInOutReal) {
    basic_rmatrix<TP> m1(3,4);
    basic_rmatrix<TP> m2(3,4);
    m1.randomize(-5., 5.);

    std::ostringstream os;
    os.precision(17);
    os.setf (std::ios::scientific | std::ios::showpoint | std::ios::left);
    os << m1;
    std::string s = os.str();

    std::istringstream is(s);
    is.precision(17);
    is >> m2;
    EXPECT_EQ(m1, m2) << "Real Matrix in/out";
}

TYPED_TEST(MiscTest, TestMatrixInOutRealComplex) {
    basic_cmatrix<TP,TPC> m1(3,4);
    basic_cmatrix<TP,TPC> m2(3,4);
    m1.randomize_real(-5., 5.);
    m1.randomize_imag(-5., 5.);

    std::ostringstream os;
    os.precision(17);
    os.setf (std::ios::scientific | std::ios::showpoint | std::ios::left);
    os << m1;
    std::string s = os.str();

    std::istringstream is(s);
    is.precision(17);
    is >> m2;
    EXPECT_EQ(m1, m2) << "Complex Matrix in/out";
}

TYPED_TEST(MiscTest, TestComplexByRealProxy) {
    TP pi = 3.1415926535897932384626433832795;
    TP ci = 1.;
    TPC phase = exp(2*pi*ci);

    basic_rmatrix<TP> tmp(2,2);
    tmp(CVM0+1,CVM0+1) = 3.;
    basic_cmatrix<TP,TPC> H(10,10);
    H(CVM0+1,CVM0+1) += phase*tmp(CVM0+1,CVM0+1);
    EXPECT_NEAR(std::abs(TPC(1.606474966574294e+03,0.)), std::abs(H(CVM0+1,CVM0+1)), sp<TP>())
        << "tcomplex * type_proxy<treal>";
}
