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

TYPED_TEST(BlasTest, TestVectorNorm1) {
    TP a[] = {1., 2., 3., -4., 5., -6.};
    const basic_rvector<TP> vr(a, 6);
    const basic_cvector<TP,TPC> vc((TPC*) a, 3);
    EXPECT_EQ(TP(21.), vr.norm1()) << "rvector::norm1";
    EXPECT_NEAR(15.04631765340644,vc.norm1(),sf<TP>()) << "cvector::norm1";
}

TYPED_TEST(BlasTest, TestMatrixSumReal) {
    TP a[] = {1., 2., 3., 4., 5., 6.};
    const basic_rmatrix<TP> m1(a, 2, 3);
    basic_rmatrix<TP> m2(2, 3);
    basic_rmatrix<TP> m(2, 3);
    m2.set(1.);
    
    EXPECT_EQ(TP(5.), m.sum(m1,m2)(CVM0+1,CVM0+1)) << "rmatrix::sum";
    EXPECT_EQ(TP(7.), m.sum(m,m2)(CVM0,CVM0+2)) << "rmatrix::sum";
}

TYPED_TEST(BlasTest, TestMatrixDiffReal) {
    TP a[] = {1., 2., 3., 4., 5., 6.};
    const basic_rmatrix<TP> m1(a, 2, 3);
    basic_rmatrix<TP> m2(2, 3);
    basic_rmatrix<TP> m(2, 3);
    m2.set(1.);
    
    EXPECT_EQ(TP(3.), m.diff(m1,m2)(CVM0+1,CVM0+1)) << "rmatrix::diff";
    EXPECT_EQ(TP(3.), m.diff(m,m2)(CVM0,CVM0+2)) << "rmatrix::diff";
}

TYPED_TEST(BlasTest, TestMatrixSumComplex) {
    TP a[] = {1., 2., 3., 4., 5., 6.,
        7., 8., 9., 10., 11., 12.};
    const basic_cmatrix<TP,TPC> ma ((TPC*) a, 2, 3);
    basic_cmatrix<TP,TPC> mb (2, 3);
    basic_cmatrix<TP,TPC> m (2, 3);
    mb.set (TPC(1.,1.));
    
    EXPECT_EQ(TPC(8.,9.),m.sum(ma, mb)(CVM0+1,CVM0+1)) << "cmatrix::sum";
    EXPECT_EQ(TPC(11.,12.),m.sum(m, mb)(CVM0,CVM0+2)) << "cmatrix::sum";
}

TYPED_TEST(BlasTest, TestMatrixDiffComplex) {
    TP a[] = {1., 2., 3., 4., 5., 6.,
        7., 8., 9., 10., 11., 12.};
    const basic_cmatrix<TP,TPC> ma ((TPC*) a, 2, 3);
    basic_cmatrix<TP,TPC> mb (2, 3);
    basic_cmatrix<TP,TPC> m (2, 3);
    mb.set (TPC(1.,1.));
    
    EXPECT_EQ(TPC(6.,7.),m.diff(ma, mb)(CVM0+1,CVM0+1)) << "cmatrix::diff";
    EXPECT_EQ(TPC(7.,8.),m.diff(m, mb)(CVM0,CVM0+2)) << "cmatrix::diff";
}

TYPED_TEST(BlasTest, TestBandMatrixSumReal) {
    TP a[] = {1., 2., 3., 4., 5., 6.};
    const basic_srbmatrix<TP> m1(a, 3, 1, 0);
    basic_srbmatrix<TP> m2(3, 1, 0);
    basic_srbmatrix<TP> m(3, 1, 0);
    m2.set(1.);
    EXPECT_EQ(TP(4.), m.sum(m1,m2)(CVM0+1,CVM0+1)) << "srbmatrix::sum";
    EXPECT_EQ(TP(0.), m(CVM0+1,CVM0+2)) << "srbmatrix::sum";
    EXPECT_EQ(TP(3.), m(CVM0+1,CVM0)) << "srbmatrix::sum";
    EXPECT_EQ(TP(4.), m.sum(m,m2)(CVM0+1,CVM0)) << "srbmatrix::sum";
    EXPECT_EQ(TP(7.), m(CVM0+2,CVM0+2)) << "srbmatrix::sum";
}

