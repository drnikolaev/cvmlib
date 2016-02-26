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
class LapackTest : public ::testing::Test {
protected:
    LapackTest() {}
    virtual ~LapackTest() {}
};

TYPED_TEST_CASE(LapackTest, TestTypes);

// Fixed syrk bug reported by Markus Jochmann.
TYPED_TEST(LapackTest, TestSyrkReal) {
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
TYPED_TEST(LapackTest, TestSyrkComplex) {
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

// MATLAB-style operator B/A returning solution of X*A=B equation which is actually A'*X'=B'
// 6.1
TYPED_TEST(LapackTest, TestSlashReal) {
    basic_srmatrix<TP> srm(7);
    basic_srbmatrix<TP> srbm(7, 1, 3);
    basic_srsmatrix<TP> srsm(7);
    basic_rvector<TP> vB(7), vX(7);
    srm.randomize(-10., 20.);
    srbm.randomize(-10., 20.);
    srsm.randomize(-10., 5.);
    vB.randomize(-10., 20.);

    vX = vB / srm;
    EXPECT_NEAR(0,(vX * srm - vB).norm(),sp<TP>()) << "rvector / srmatrix";
    vX = srm % vB;
    EXPECT_NEAR(0,(vX * srm - vB).norm(),sp<TP>()) << "srmatrix % rvector";

    vX = vB / srbm;
    EXPECT_NEAR(0,(vX * srbm - vB).norm(),spp<TP>(1.e-10,0.01)) << "rvector / srbmatrix";
    vX = srbm % vB;
    EXPECT_NEAR(0,(vX * srbm - vB).norm(),spp<TP>(1.e-10,0.01)) << "srbmatrix % rvector";

    vX = vB / srsm;
    EXPECT_NEAR(0,(vX * srsm - vB).norm(),sp<TP>()) << "rvector / srsmatrix";
    vX = srsm % vB;
    EXPECT_NEAR(0,(vX * srsm - vB).norm(),sp<TP>()) << "srsmatrix % rvector";

    vX = vB % srm;
    EXPECT_NEAR(0,(srm * vX - vB).norm(),sp<TP>()) << "rvector % srmatrix";
    vX = srm / vB;
    EXPECT_NEAR(0,(srm * vX - vB).norm(),sp<TP>()) << "srmatrix / rvector";

    vX = vB % srbm;
    EXPECT_NEAR(0,(srbm * vX - vB).norm(),sp<TP>()) << "rvector % srbmatrix";
    vX = srbm / vB;
    EXPECT_NEAR(0,(srbm * vX - vB).norm(),sp<TP>()) << "srbmatrix / rvector";

    vX = vB % srsm;
    EXPECT_NEAR(0,(srsm * vX - vB).norm(),sp<TP>()) << "rvector % srsmatrix";
    vX = srsm / vB;
    EXPECT_NEAR(0,(srsm * vX - vB).norm(),sp<TP>()) << "srsmatrix / rvector";
}

// 6.1
TYPED_TEST(LapackTest, TestSlashComplex) {
    basic_scmatrix<TP,TPC> scm(7);
    basic_scbmatrix<TP,TPC> scbm(7, 1, 3);
    basic_schmatrix<TP,TPC> schm(7);
    basic_cvector<TP,TPC> vB(7), vX(7);
    scm.randomize_real(-10., 20.);
    scm.randomize_imag(-10., 20.);
    scbm.randomize_real(-10., 20.);
    scbm.randomize_imag(-10., 20.);
    schm.randomize_real(-10., 20.);
    schm.randomize_imag(-10., 20.);
    vB.randomize_real(-10., 20.);
    vB.randomize_imag(-10., 20.);

    vX = vB / scm;
    EXPECT_NEAR(0,(vX * scm - vB).norm(),sp<TP>()) << "cvector / scmatrix";
    vX = scm % vB;
    EXPECT_NEAR(0,(vX * scm - vB).norm(),sp<TP>()) << "scmatrix % cvector";

    vX = vB / scbm;
    EXPECT_NEAR(0,(vX * scbm - vB).norm(),sp<TP>()) << "cvector / scbmatrix";
    vX = scbm % vB;
    EXPECT_NEAR(0,(vX * scbm - vB).norm(),sp<TP>()) << "scbmatrix % cvector";

    vX = vB / schm;
    EXPECT_NEAR(0,(vX * schm - vB).norm(),sp<TP>()) << "cvector / schmatrix";
    vX = schm % vB;
    EXPECT_NEAR(0,(vX * schm - vB).norm(),sp<TP>()) << "schmatrix % cvector";

    vX = vB % scm;
    EXPECT_NEAR(0,(scm * vX - vB).norm(),sp<TP>()) << "scmatrix % cvector";
    vX = scm / vB;
    EXPECT_NEAR(0,(scm * vX - vB).norm(),sp<TP>()) << "scmatrix / cvector";

    vX = vB % scbm;
    EXPECT_NEAR(0,(scbm * vX - vB).norm(),sp<TP>()) << "cvector % scbmatrix";
    vX = scbm / vB;
    EXPECT_NEAR(0,(scbm * vX - vB).norm(),sp<TP>()) << "scbmatrix / cvector";

    vX = vB % schm;
    EXPECT_NEAR(0,(schm * vX - vB).norm(),sp<TP>()) << "cvector % schmatrix";
    vX = schm / vB;
    EXPECT_NEAR(0,(schm * vX - vB).norm(),sp<TP>()) << "schmatrix / cvector";
}

