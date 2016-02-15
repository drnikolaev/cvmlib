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
class Lapack1Test : public ::testing::Test {
protected:
    Lapack1Test() {}
    virtual ~Lapack1Test() {}
};

TYPED_TEST_CASE(Lapack1Test, TestTypes);

// Fixed syrk bug reported by Markus Jochmann.
TYPED_TEST(Lapack1Test, TestSyrkReal) {
    basic_srsmatrix<TP> mat1(2), mat2(2);
    basic_rmatrix<TP> v(2,2);
    v(CVM0,CVM0) = 1;
    v(CVM0,CVM0+1) = 2;
    v(CVM0+1,CVM0) = 3;
    v(CVM0+1,CVM0+1) = 4;

    basic_rvector<TP> vrow(2);
    vrow = v[CVM0];
    mat1.syrk(1.0, v[CVM0], 0.0);
    mat2.syrk(1.0, vrow, 0.0);

    EXPECT_EQ(0., (mat1 - mat2).norm()) << "srsmatrix::syrk with incr=2";

    mat1.syr2k(1.0, v[CVM0], v[CVM0], 0.0);
    mat2.syr2k(1.0, vrow, vrow, 0.0);
    EXPECT_EQ(0., (mat1 - mat2).norm()) << "srsmatrix::syr2k with incr=2";
}

// Fixed syrk bug reported by Markus Jochmann.
TYPED_TEST(Lapack1Test, TestSyrkComplex) {
    basic_schmatrix<TP,TPC> mat1(2), mat2(2);
    basic_cmatrix<TP,TPC> v(2,2);
    v(CVM0,CVM0) = TPC(1.,1.);
    v(CVM0,CVM0+1) = TPC(2.,2.);
    v(CVM0+1,CVM0) = TPC(3.,3.);
    v(CVM0+1,CVM0+1) = TPC(4.,4.);

    basic_cvector<TP,TPC> vrow(2);
    vrow = v[CVM0];

    TPC c1 (1.43, -0.391);
    TP r1(1.17), r2(-0.632);

    mat1.herk(r1, v[CVM0], r2);
    mat2.herk(r1, vrow, r2);
    EXPECT_EQ(0., (mat1 - mat2).norm()) << "schmatrix::herk with incr=2";

    mat1.her2k(c1, v[CVM0], v[CVM0], r2);
    mat2.her2k(c1, vrow, vrow, r2);
    EXPECT_EQ(0., (mat1 - mat2).norm()) << "schmatrix::her2k with incr=2";
}

