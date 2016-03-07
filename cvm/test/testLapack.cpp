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
    EXPECT_NEAR(0,(vX * schm - vB).norm(),spp<TP>()) << "cvector / schmatrix";
    vX = schm % vB;
    EXPECT_NEAR(0,(vX * schm - vB).norm(),spp<TP>()) << "schmatrix % cvector";

    vX = vB % scm;
    EXPECT_NEAR(0,(scm * vX - vB).norm(),sp<TP>()) << "scmatrix % cvector";
    vX = scm / vB;
    EXPECT_NEAR(0,(scm * vX - vB).norm(),sp<TP>()) << "scmatrix / cvector";

    vX = vB % scbm;
    EXPECT_NEAR(0,(scbm * vX - vB).norm(),sp<TP>()) << "cvector % scbmatrix";
    vX = scbm / vB;
    EXPECT_NEAR(0,(scbm * vX - vB).norm(),sp<TP>()) << "scbmatrix / cvector";

    vX = vB % schm;
    EXPECT_NEAR(0,(schm * vX - vB).norm(),spp<TP>()) << "cvector % schmatrix";
    vX = schm / vB;
    EXPECT_NEAR(0,(schm * vX - vB).norm(),spp<TP>()) << "schmatrix / cvector";
}

TYPED_TEST(LapackTest, TestCholeskySymmetricReal) {
    TP a[] = {1., 2., 1., 2., 5., -1., 1., -1., 20.};
    const basic_srsmatrix<TP> m(a, 3);
    basic_srmatrix<TP> h = m.cholesky();
    EXPECT_NEAR(TP(0.), (~h * h - m).norm(), sp<TP>()) << "srsmatrix::cholesky";
}

TYPED_TEST(LapackTest, TestCholeskyHermitianComplex) {
    TP a[] = {3., 0., 2., 1., -1., 2., 2., -1., 3., 0.,
              0., 3., -1., -2., 0., -3., 5., 0.};
    const basic_schmatrix<TP,TPC> m{(TPC*)a, 3};
    basic_scmatrix<TP,TPC> h = m.cholesky();
    EXPECT_NEAR(0., (~h * h - m).norm(), sp<TP>()) << "schmatrix::cholesky";
}

TYPED_TEST(LapackTest, TestVEigSymmetricRealHermitianComplex) {
    basic_srsmatrix<TP> m(3);
    basic_srmatrix<TP> me(3);
    basic_rvector<TP> v(3);
    m.randomize(1., 3.);
    
    v.eig (m, me);
    EXPECT_NEAR(TP(0.), (m * me(CVM0) - me(CVM0) * v(CVM0)).norm(), sf<TP>()) << "srsmatrix::eig";
    EXPECT_NEAR(TP(0.), (m * me(CVM0+1) - me(CVM0+1) * v(CVM0+1)).norm(), sf<TP>()) << "srsmatrix::eig";
    EXPECT_NEAR(TP(0.), (m * me(CVM0+2) - me(CVM0+2) * v(CVM0+2)).norm(), sf<TP>()) << "srsmatrix::eig";
    
    EXPECT_NEAR(TP(0.), me(CVM0) * me(CVM0+1), sf<TP>()) << "srsmatrix::eig";
    EXPECT_NEAR(TP(0.), me(CVM0) * me(CVM0+2), sf<TP>()) << "srsmatrix::eig";
    EXPECT_NEAR(TP(0.), me(CVM0+2) * me(CVM0+1), sf<TP>()) << "srsmatrix::eig";
    
    basic_schmatrix<TP,TPC> mc(3);
    basic_scmatrix<TP,TPC> mce(3);
    mc.randomize_real(1., 3.);
    mc.randomize_imag(1., 3.);
    
    v.eig (mc, mce);
    
    EXPECT_NEAR(TP(0.), (mc * mce(CVM0) - mce(CVM0) * v(CVM0)).norm(), sf<TP>()) << "schmatrix::eig";
    EXPECT_NEAR(TP(0.), (mc * mce(CVM0+1) - mce(CVM0+1) * v(CVM0+1)).norm(), sf<TP>()) << "schmatrix::eig";
    EXPECT_NEAR(TP(0.), (mc * mce(CVM0+2) - mce(CVM0+2) * v(CVM0+2)).norm(), sf<TP>()) << "schmatrix::eig";
    
    EXPECT_NEAR(std::abs(TPC(0.,0.)), std::abs(mce(CVM0) % mce(CVM0+1)), sf<TP>()) << "srsmatrix::eig";
    EXPECT_NEAR(std::abs(TPC(0.,0.)), std::abs(mce(CVM0) % mce(CVM0+2)), sf<TP>()) << "srsmatrix::eig";
    EXPECT_NEAR(std::abs(TPC(0.,0.)), std::abs(mce(CVM0+2) % mce(CVM0+1)), sf<TP>()) << "srsmatrix::eig";
}

TYPED_TEST(LapackTest, TestEigSymmetricReal) {
    TP a[] = {1., 2., 1., 2., 0., -1., 1., -1., 2.};
    const basic_srsmatrix<TP> m{a, 3};
    basic_srmatrix<TP> me(3);
    basic_rvector<TP> v(3);
    v = m.eig(me);
    EXPECT_NEAR(TP(0.), (m * me(CVM0) - me(CVM0) * v(CVM0)).norm(), sf<TP>()) << "srsmatrix::eig";
    EXPECT_NEAR(TP(0.), (m * me(CVM0+1) - me(CVM0+1) * v(CVM0+1)).norm(), sf<TP>()) << "srsmatrix::eig";
    EXPECT_NEAR(TP(0.), (m * me(CVM0+2) - me(CVM0+2) * v(CVM0+2)).norm(), sf<TP>()) << "srsmatrix::eig";
}

TYPED_TEST(LapackTest, TestEigHermitianComplex) {
    TP a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
              0., 3., -1., -2., 0., -3., 3., 0.};
    basic_schmatrix<TP,TPC> m{(TPC*)a, 3};
    basic_scmatrix<TP,TPC> me{3};
    basic_rvector<TP> v(3);
    v = m.eig(me);
    basic_cvector<TP,TPC> vc{v};
    EXPECT_NEAR(TP(0.), (m * me(CVM0) - me(CVM0) * vc(CVM0)).norm(), sf<TP>()) << "schmatrix::eig";
    EXPECT_NEAR(TP(0.), (m * me(CVM0+1) - me(CVM0+1) * vc(CVM0+1)).norm(), sf<TP>()) << "schmatrix::eig";
    EXPECT_NEAR(TP(0.), (m * me(CVM0+2) - me(CVM0+2) * vc(CVM0+2)).norm(), sf<TP>()) << "schmatrix::eig";
    EXPECT_NEAR(std::abs(TPC(0.,0.)), std::abs(me(CVM0) % me(CVM0+1)), sf<TP>()) << "schmatrix::eig";
    EXPECT_NEAR(std::abs(TPC(0.,0.)), std::abs(me(CVM0+1) % me(CVM0+2)), sf<TP>()) << "schmatrix::eig";
    EXPECT_NEAR(std::abs(TPC(0.,0.)), std::abs(me(CVM0) % me(CVM0+2)), sf<TP>()) << "schmatrix::eig";
}

TYPED_TEST(LapackTest, TestInvHermitianComplex) {
    TP a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
              0., 3., -1., -2., 0., -3., 3., 0.};
    basic_schmatrix<TP,TPC> m{(TPC*)a, 3};
    const basic_schmatrix<TP,TPC> mi = m.inv();
    EXPECT_NEAR(TP(0.), (mi * m - basic_eye_complex<TP,TPC>(3)).norm(), sp<TP>()) << "schmatrix::inv";
}

TYPED_TEST(LapackTest, TestSyrkSymmetricReal1) {
    TP a[] = {1., 2., 3., 4.};
    basic_rvector<TP> v(a, 4);
    basic_srsmatrix<TP> ms(4);
    ms.set(1.);
    ms.syrk (2., v, 1.);
    EXPECT_NEAR(TP(33.), ms(CVM0+3, CVM0+3), s<TP>()) << "srsmatrix::syrk";
    EXPECT_NEAR(TP(9.), ms(CVM0, CVM0+3), s<TP>()) << "srsmatrix::syrk";
    
    basic_rmatrix<TP> m(4, 2);
    m(CVM0) = v;
    m(CVM0+1).set(1.);
    ms.syrk (false, 2., m, 0.);
    EXPECT_NEAR(TP(34.), ms(CVM0+3, CVM0+3), s<TP>()) << "srsmatrix::syrk";
    EXPECT_NEAR(TP(10.), ms(CVM0, CVM0+3), s<TP>()) << "srsmatrix::syrk";
    
    basic_srsmatrix<TP> ms2(2);
    ms2.syrk (true, 1., m, 0.);
    EXPECT_NEAR(TP(30.), ms2(CVM0, CVM0), s<TP>()) << "srsmatrix::syrk";
    EXPECT_NEAR(TP(10.), ms2(CVM0, CVM0+1), s<TP>()) << "srsmatrix::syrk";
    EXPECT_NEAR(TP(4.), ms2(CVM0+1, CVM0+1), s<TP>()) << "srsmatrix::syrk";
}

TYPED_TEST(LapackTest, TestSyrkSymmetricReal2) {
    const TP alpha = 2.12;
    const TP beta = -3.07;
    basic_rvector<TP> v(3);
    basic_srsmatrix<TP> ms(3), ms2(3);
    v.randomize(-3., 2.);
    ms.randomize(-1., 2.);
    ms2 = ms;
    ms.syrk (alpha, v, beta);
    ms2 = alpha * basic_srsmatrix<TP>(v.rank1update(v)) + beta * ms2;
    EXPECT_NEAR(TP(0.), (ms - ms2).norm(), spp<TP>(1.e-14,2.)) << "srsmatrix::syrk";
}

TYPED_TEST(LapackTest, TestSyrkSymmetricReal3) {
    const TP alpha = 2.12;
    const TP beta = -3.07;
    basic_rmatrix<TP> m(3, 3);
    basic_srsmatrix<TP> ms(3), ms2(3);
    m.randomize(-1., 2.);
    ms.randomize(-1., 2.);
    ms2 = ms;
    ms.syrk (false, alpha, m, beta);
    ms2 = alpha * basic_srsmatrix<TP>(m * ~m) + beta * ms2;
    EXPECT_NEAR(TP(0.), (ms - ms2).norm(), sf<TP>()) << "srsmatrix::syrk";
}

TYPED_TEST(LapackTest, TestSyrkSymmetricReal4) {
    const TP alpha = 2.12;
    const TP beta = -3.07;
    basic_rmatrix<TP> m(3, 3);
    basic_srsmatrix<TP> ms(3), ms2(3);
    m.randomize(-1., 2.);
    ms.randomize(-1., 2.);
    ms2 = ms;
    ms.syrk (true, alpha, m, beta);
    ms2 = alpha * basic_srsmatrix<TP>(~m * m) + beta * ms2;
    EXPECT_NEAR(TP(0.), (ms - ms2).norm(), s<TP>()) << "srsmatrix::syrk";
}

TYPED_TEST(LapackTest, TestHerkHermitianComplex1) {
    const TP a[] = {1., -1., 2., 2., 3., -3.};
    const basic_cvector<TP,TPC> v{(TPC*)a, 3};
    const TP alpha = 2.12;
    const TP beta = -3.07;
    basic_schmatrix<TP,TPC> mh(3), mh2(3);
    mh.randomize_real(-1., 2.);
    mh.randomize_imag(-2., 3.);
    mh2 = mh;
    mh.herk (alpha, v, beta);
    mh2 = alpha * basic_schmatrix<TP,TPC>(v.rank1update_c(v)) + beta * mh2;
    EXPECT_NEAR(TP(0.), (mh - mh2).norm(), spp<TP>(1.e-14, 2.0)) << "schmatrix::herk";
}

TYPED_TEST(LapackTest, TestHerkHermitianComplex2) {
    basic_cmatrix<TP,TPC> m(3, 3);
    const TP alpha = 2.12;
    const TP beta = -3.07;
    basic_schmatrix<TP,TPC> mh(3), mh2(3);
    m.randomize_real(-1., 2.);
    m.randomize_imag(-2., 3.);
    mh.randomize_real(-1., 2.);
    mh.randomize_imag(-2., 3.);
    mh2 = mh;
    mh.herk (false, alpha, m, beta);
    mh2 = alpha * basic_schmatrix<TP,TPC>(m * ~m, 1.e-14) + beta * mh2;
    EXPECT_NEAR(TP(0.), (mh - mh2).norm(), spp<TP>(2.e-14, 6.)) << "schmatrix::herk";
}

TYPED_TEST(LapackTest, TestHerkHermitianComplex3) {
    basic_cmatrix<TP,TPC> m(3, 3);
    const TP alpha = 2.12;
    const TP beta = -3.07;
    basic_schmatrix<TP,TPC> mh(3), mh2(3);
    m.randomize_real(-1., 2.);
    m.randomize_imag(-2., 3.);
    mh.randomize_real(-1., 2.);
    mh.randomize_imag(-2., 3.);
    mh2 = mh;
    mh.herk (true, alpha, m, beta);
    mh2 = alpha * basic_schmatrix<TP,TPC>(~m * m, 1.e-14) + beta * mh2;
    EXPECT_NEAR(TP(0.), (mh - mh2).norm(), spp<TP>(1.e-14, 6.)) << "schmatrix::herk";
}

TYPED_TEST(LapackTest, TestSyr2kSymmetricReal1) {
    TP a1[] = {1., 2., 3., 4.};
    TP a2[] = {1., 2., 3., 4.};
    basic_rvector<TP> v1(a1, 4);
    basic_rvector<TP> v2(a2, 4);
    basic_srsmatrix<TP> ms(4);
    ms.set(1.);
    ms.syr2k (2., v1, v2, 1.);
    EXPECT_NEAR(TP(65.), ms(CVM0+3, CVM0+3), s<TP>()) << "srsmatrix::syr2k";
    EXPECT_NEAR(TP(17.), ms(CVM0, CVM0+3), s<TP>()) << "srsmatrix::syr2k";
    
    basic_rmatrix<TP> m1(4, 2);
    basic_rmatrix<TP> m2(4, 2);
    m1.set(1.);
    m2.set(2.);
    ms.syr2k (false, 2., m1, m2, 0.);
    EXPECT_NEAR(TP(16.), ms(CVM0+3, CVM0+3), s<TP>()) << "srsmatrix::syr2k";
    EXPECT_NEAR(TP(16.), ms(CVM0, CVM0+3), s<TP>()) << "srsmatrix::syr2k";
    
    basic_srsmatrix<TP> ms2(2);
    ms2.syr2k (true, 1., m1, m2, 0.);
    EXPECT_NEAR(TP(16.), ms2(CVM0+1, CVM0+1), s<TP>()) << "srsmatrix::syr2k";
    EXPECT_NEAR(TP(16.), ms2(CVM0, CVM0+1), s<TP>()) << "srsmatrix::syr2k";
}

TYPED_TEST(LapackTest, TestSyr2kSymmetricReal2) {
    const TP alpha = 2.12;
    const TP beta = -3.07;
    basic_rvector<TP> v1(4);
    basic_rvector<TP> v2(4);
    basic_srsmatrix<TP> ms(4), ms2(4);
    v1.randomize(-1., 3.);
    v2.randomize(-1., 3.);
    ms.randomize(-1., 3.);
    ms2 = ms;
    ms.syr2k (alpha, v1, v2, beta);
    ms2 = alpha * basic_srsmatrix<TP>(v1.rank1update(v2) + v2.rank1update(v1)) + beta * ms2;
    EXPECT_NEAR(TP(0.), (ms - ms2).norm(), sf<TP>()) << "srsmatrix::syr2k";
}

TYPED_TEST(LapackTest, TestSyr2kSymmetricReal3) {
    const TP alpha = 2.12;
    const TP beta = -3.07;
    basic_rmatrix<TP> m1(3, 3), m2(3, 3);
    basic_srsmatrix<TP> ms(3), ms2(3);
    m1.randomize(-2., 2.);
    m2.randomize(-2., 2.);
    ms.randomize(-1., 2.);
    ms2 = ms;
    ms.syr2k (false, alpha, m1, m2, beta);
    ms2 = alpha * basic_srsmatrix<TP>(m1 * ~m2 + m2 * ~m1) + beta * ms2;
    EXPECT_NEAR(TP(0.), (ms - ms2).norm(), sf<TP>()) << "srsmatrix::syr2k";
}

TYPED_TEST(LapackTest, TestSyr2kSymmetricReal4) {
    const TP alpha = 2.12;
    const TP beta = -3.07;
    basic_rmatrix<TP> m1(3, 3), m2(3, 3);
    basic_srsmatrix<TP> ms(3), ms2(3);
    m1.randomize(-2., 2.);
    m2.randomize(-2., 2.);
    ms.randomize(-1., 2.);
    ms2 = ms;
    ms.syr2k (true, alpha, m1, m2, beta);
    ms2 = alpha * basic_srsmatrix<TP>(~m1 * m2 + ~m2 * m1) + beta * ms2;
    EXPECT_NEAR(TP(0.), (ms - ms2).norm(), sf<TP>()) << "srsmatrix::syr2k";
}

TYPED_TEST(LapackTest, TestHer2kHermitianComplex1) {
    const TPC alpha = TPC(2.12, -0.14);
    const TPC alphac = TPC(2.12, 0.14);
    const TP beta = -3.07;
    basic_cvector<TP,TPC> v1(3), v2(3);
    basic_schmatrix<TP,TPC> mh(3), mh2(3);
    v1.randomize_real(-1., 2.);
    v1.randomize_imag(-2., 3.);
    v2.randomize_real(-1., 2.);
    v2.randomize_imag(-2., 3.);
    mh.randomize_real(-1., 2.);
    mh.randomize_imag(-2., 3.);
    mh2 = mh;
    mh.her2k(alpha, v1, v2, beta);
    mh2 = basic_schmatrix<TP,TPC>(alpha * v1.rank1update_c(v2) +
                                  alphac * v2.rank1update_c(v1),
                                  spp<TP>(1.5e-14, 1.5e-5)) + beta * mh2;
    EXPECT_NEAR(TP(0.), (mh - mh2).norm(), spp<TP>(1.5e-14, 0.5)) << "schmatrix::her2k";
}

TYPED_TEST(LapackTest, TestHer2kHermitianComplex2) {
    const TPC alpha = TPC(2.12, -0.14);
    const TPC alphac = TPC(2.12, 0.14);
    const TP beta = -3.07;
    basic_cmatrix<TP,TPC> m1(3, 3), m2(3, 3);
    basic_schmatrix<TP,TPC> mh(3), mh2(3);
    m1.randomize_real(-1., 2.);
    m1.randomize_imag(-2., 3.);
    m2.randomize_real(-1., 2.);
    m2.randomize_imag(-2., 3.);
    mh.randomize_real(-1., 2.);
    mh.randomize_imag(-2., 3.);
    mh2 = mh;
    mh.her2k (false, alpha, m1, m2, beta);
    mh2 = basic_schmatrix<TP,TPC>(alpha * m1 * ~m2 +
                                  alphac * m2 * ~m1,
                                  spp<TP>(1.5e-14, 1.5e-5)) + beta * mh2;
    EXPECT_NEAR(TP(0.), (mh - mh2).norm(), spp<TP>(2.e-14, 0.6)) << "schmatrix::her2k";
}

TYPED_TEST(LapackTest, TestHer2kHermitianComplex3) {
    const TPC alpha = TPC(2.12, -0.14);
    const TPC alphac = TPC(2.12, 0.14);
    const TP beta = -3.07;
    basic_cmatrix<TP,TPC> m1(3, 3), m2(3, 3);
    basic_schmatrix<TP,TPC> mh(3), mh2(3);
    m1.randomize_real(-1., 2.);
    m1.randomize_imag(-2., 3.);
    m2.randomize_real(-1., 2.);
    m2.randomize_imag(-2., 3.);
    mh.randomize_real(-1., 2.);
    mh.randomize_imag(-2., 3.);
    mh2 = mh;
    mh.her2k(true, alpha, m1, m2, beta);
    mh2 = basic_schmatrix<TP,TPC>(alpha * ~m1 * m2 +
                                  alphac * ~m2 * m1,
                                  spp<TP>(1.5e-14, 1.5e-5)) + beta * mh2;
    EXPECT_NEAR(TP(0.), (mh - mh2).norm(), spp<TP>(2.e-14, 0.5)) << "schmatrix::her2k";
}

TYPED_TEST(LapackTest, TestSolveLUReal) {
    TP a[] = {1., -1., 1., 2., -2., 1., 3., -2., 1.};
    basic_srmatrix<TP> ma(a, 3);
    basic_srmatrix<TP> mLU(3);
    basic_rmatrix<TP> mb1(3, 2);
    basic_rvector<TP> vb1(3);
    basic_rmatrix<TP> mb2(3, 2);
    basic_rvector<TP> vb2(3);
    basic_rmatrix<TP> mx1(3, 2);
    basic_rvector<TP> vx1(3);
    basic_rmatrix<TP> mx2(3, 2);
    basic_rvector<TP> vx2(3);
    iarray   nPivots(3);
    TP   dErr = 0.;
    mb1.randomize(-1., 3.);
    vb1.randomize(-2., 4.);
    mb2.randomize(-2., 5.);
    vb2.randomize(-3., 1.);
    
    mLU.low_up(ma, nPivots);
    mx1 = ma.solve_lu (mLU, nPivots, mb1, dErr);
    EXPECT_NEAR(TP(0.), dErr, sf<TP>()) << "rmatrix::solve_lu";
    
    mx2 = ma.solve_lu (mLU, nPivots, mb2);
    EXPECT_NEAR(TP(0.), (ma * mx1 - mb1).norm(), sf<TP>()) << "rmatrix::solve_lu";
    EXPECT_NEAR(TP(0.), (ma * mx2 - mb2).norm(), sf<TP>()) << "rmatrix::solve_lu";
    
    vx1 = ma.solve_lu (mLU, nPivots, vb1, dErr);
    EXPECT_NEAR(TP(0.), dErr, sf<TP>()) << "rmatrix::solve_lu";
    vx2 = ma.solve_lu (mLU, nPivots, vb2);
    EXPECT_NEAR(TP(0.), (ma * vx1 - vb1).norm(), sf<TP>()) << "rmatrix::solve_lu";
    EXPECT_NEAR(TP(0.), (ma * vx2 - vb2).norm(), sf<TP>()) << "rmatrix::solve_lu";
}

TYPED_TEST(LapackTest, TestSolveLUReal2) {
    TP m[] = {1., -1., 1., 2., -2., 1., 3., -2., 1.};
    TP b1[] = {1., 2., 3.};
    TP b2[] = {0., -1., -2.};
    basic_srmatrix<TP> ma(m, 3);
    basic_srmatrix<TP> mLU(3);
    basic_rvector<TP>  vb1(b1, 3);
    basic_rvector<TP>  vb2(b2, 3);
    basic_rvector<TP>  vx1(3);
    basic_rvector<TP>  vx2(3);
    iarray   nPivots(3);
    TP   dErr = 0.;
    
    mLU.low_up(ma, nPivots);
    vx1.solve_lu (ma, mLU, nPivots, vb1, dErr);
    EXPECT_NEAR(TP(0.), dErr, sf<TP>()) << "rmatrix::solve_lu";
    vx2.solve_lu (ma, mLU, nPivots, vb2);
    EXPECT_NEAR(TP(0.), (ma * vx1 - vb1).norm(), sf<TP>()) << "rmatrix::solve_lu";
    EXPECT_NEAR(TP(0.), (ma * vx2 - vb2).norm(), sf<TP>()) << "rmatrix::solve_lu";
}

TYPED_TEST(LapackTest, TestSolveLUComplex) {
    basic_scmatrix<TP,TPC> ma(3);
    basic_scmatrix<TP,TPC> mLU(3);
    basic_cmatrix<TP,TPC> mb1(3, 2);
    basic_cmatrix<TP,TPC> mb2(3, 2);
    basic_cmatrix<TP,TPC> mx1(3, 2);
    basic_cmatrix<TP,TPC> mx2(3, 2);
    iarray nPivots(3);
    TP dErr = 0.;
    ma.randomize_real(0., 10.);
    ma.randomize_imag(0., 10.);
    mb1.randomize_real(0., 10.);
    mb1.randomize_imag(0., 10.);
    mb2.randomize_real(0., 10.);
    mb2.randomize_imag(0., 10.);
    
    mLU.low_up(ma, nPivots);
    mx1.solve_lu (ma, mLU, nPivots, mb1, dErr);
    EXPECT_NEAR(TP(0.), dErr, sp<TP>()) << "cmatrix::solve_lu";
    mx2.solve_lu (ma, mLU, nPivots, mb2);
    EXPECT_NEAR(TP(0.), (ma * mx1 - mb1).norm(), sf<TP>()) << "cmatrix::solve_lu";
    EXPECT_NEAR(TP(0.), (ma * mx2 - mb2).norm(), sf<TP>()) << "cmatrix::solve_lu";
}

TYPED_TEST(LapackTest, TestSymmReal) {
    TP alpha = 1.3;
    TP beta = -0.7;
    basic_rmatrix<TP> m1(3, 4);
    basic_rmatrix<TP> m2(4, 3);
    basic_srsmatrix<TP> ms(3);
    basic_rmatrix<TP> m(3, 4);
    m.randomize(-1., 2.);
    m1.randomize(-1., 3.);
    m2.randomize(0., 2.);
    ms.randomize(-3., 1.);
    
    basic_rmatrix<TP> mr1 = ms * m1 * alpha + m * beta;
    EXPECT_NEAR(TP(0.), (mr1 - m.symm (true, ms, m1, alpha, beta)).norm(), sf<TP>()) << "rmatrix::symm";
    
    m.resize(4, 3);
    basic_rmatrix<TP> mr2 = m2 * ms * alpha + m * beta;
    EXPECT_NEAR(TP(0.), (mr2 - m.symm (false, ms, m2, alpha, beta)).norm(), sf<TP>()) << "rmatrix::symm";
}

TYPED_TEST(LapackTest, TestSolveLUBandReal) {
    TP a[] = {1., 2., 3., 4., 5., 6., 7., 8.};
    basic_srbmatrix<TP> ma(a, 4, 1, 0);
    basic_srbmatrix<TP> mLU(4, 1, 0);
    basic_rmatrix<TP> mb1(4, 2);
    basic_rvector<TP> vb1(4);
    basic_rmatrix<TP> mb2(4, 2);
    basic_rvector<TP> vb2(4);
    basic_rmatrix<TP> mx1(4, 2);
    basic_rvector<TP> vx1(4);
    basic_rmatrix<TP> mx2(4, 2);
    basic_rvector<TP> vx2(4);
    iarray nPivots(4);
    TP dErr = 0.;
    mb1.randomize(-1., 3.); vb1.randomize(-2., 4.);
    mb2.randomize(-2., 5.); vb2.randomize(-3., 1.);
    
    mLU.low_up(ma, nPivots);
    mx1 = ma.solve_lu (mLU, nPivots, mb1, dErr);
    EXPECT_NEAR(TP(0.), dErr, sf<TP>()) << "srbmatrix::solve_lu";
    mx2 = ma.solve_lu (mLU, nPivots, mb2);
    EXPECT_NEAR(TP(0.), (ma * mx1 - mb1).norm(), sf<TP>()) << "srbmatrix::solve_lu";
    EXPECT_NEAR(TP(0.), (ma * mx2 - mb2).norm(), sf<TP>()) << "srbmatrix::solve_lu";
    
    vx1 = ma.solve_lu (mLU, nPivots, vb1, dErr);
    EXPECT_NEAR(TP(0.), dErr, sf<TP>()) << "srbmatrix::solve_lu";
    vx2 = ma.solve_lu (mLU, nPivots, vb2);
    EXPECT_NEAR(TP(0.), (ma * vx1 - vb1).norm(), sf<TP>()) << "srbmatrix::solve_lu";
    EXPECT_NEAR(TP(0.), (ma * vx2 - vb2).norm(), sf<TP>()) << "srbmatrix::solve_lu";
}

TYPED_TEST(LapackTest, TestSolveLUBandComplex) {
    TP a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12.};
    basic_scbmatrix<TP,TPC> ma((TPC*)a, 3, 1, 0);
    basic_scbmatrix<TP,TPC> mLU(3, 1, 0);
    basic_cmatrix<TP,TPC>  mb1(3, 2);
    basic_cvector<TP,TPC> vb1(3);
    basic_cmatrix<TP,TPC>  mb2(3, 2);
    basic_cvector<TP,TPC> vb2(3);
    basic_cmatrix<TP,TPC>  mx1(3, 2);
    basic_cvector<TP,TPC> vx1(3);
    basic_cmatrix<TP,TPC>  mx2(3, 2);
    basic_cvector<TP,TPC> vx2(3);
    iarray nPivots(3);
    TP dErr = 0.;
    mb1.randomize_real(-1., 3.); mb1.randomize_imag(1., 5.);
    mb2.randomize_real(-2., 5.); mb2.randomize_imag(-3., 0.);
    vb1.randomize_real(-2., 4.); vb1.randomize_imag(-4., 1.);
    vb2.randomize_real(-3., 1.); vb2.randomize_imag(4., 5.);
    
    mLU.low_up(ma, nPivots);
    mx1 = ma.solve_lu (mLU, nPivots, mb1, dErr);
    EXPECT_NEAR(TP(0.), dErr, sf<TP>()) << "scbmatrix::solve_lu";
    mx2 = ma.solve_lu (mLU, nPivots, mb2);
    EXPECT_NEAR(TP(0.), (ma * mx1 - mb1).norm(), sf<TP>()) << "scbmatrix::solve_lu";
    EXPECT_NEAR(TP(0.), (ma * mx2 - mb2).norm(), sf<TP>()) << "scbmatrix::solve_lu";
    
    vx1 = ma.solve_lu (mLU, nPivots, vb1, dErr);
    vx2 = ma.solve_lu (mLU, nPivots, vb2);
    EXPECT_NEAR(TP(0.), (ma * vx1 - vb1).norm(), sf<TP>()) << "scbmatrix::solve_lu";
    EXPECT_NEAR(TP(0.), (ma * vx2 - vb2).norm(), sf<TP>()) << "scbmatrix::solve_lu";
}

TYPED_TEST(LapackTest, TestGemmComplex) {
    TPC alpha = TPC(1.1,2.1);
    TPC beta = TPC(0.71,0.12);
    basic_cmatrix<TP,TPC> m1(4, 3);
    basic_cmatrix<TP,TPC> m2(4, 3);
    basic_cmatrix<TP,TPC> m(3, 3);
    m.randomize_real(-1., 2.);
    m1.randomize_real(-1., 3.);
    m2.randomize_real(0., 2.);
    m.randomize_imag(1., 3.);
    m1.randomize_imag(-2., 4.);
    m2.randomize_imag(-3., 2.);
    basic_cmatrix<TP,TPC> mr = ~m1 * m2 * alpha + m * beta;
    EXPECT_NEAR(TP(0.), (mr - m.gemm(m1, true, m2, false, alpha, beta)).norm(),
                s<TP>()) << "cmatrix::gemm";
}

TYPED_TEST(LapackTest, TestHemmComplex) {
    TPC alpha = TPC(1.3,0.21);
    TPC beta = TPC(0.5,-0.1);
    basic_cmatrix<TP,TPC> m1(2, 3);
    basic_cmatrix<TP,TPC> m2(3, 2);
    basic_schmatrix<TP,TPC> ms(2);
    basic_cmatrix<TP,TPC> m(2, 3);
    m.randomize_real(-1., 2.);
    m.randomize_imag(1., 3.);
    m1.randomize_real(-1., 3.);
    m1.randomize_imag(1., 2.);
    m2.randomize_real(0., 2.);
    m2.randomize_imag(-3., -1.);
    ms.randomize_real(-3., 1.);
    ms.randomize_imag(-1.3, 4.);
    basic_cmatrix<TP,TPC> mr = ms * m1 * alpha + m * beta;
    EXPECT_NEAR(TP(0.), (mr - m.hemm (true, ms, m1, alpha, beta)).norm(), sf<TP>()) << "cmatrix::hemm";
    m.resize(3, 2);
    m.randomize_real(-1.4, 1.3); m.randomize_imag(1.1, 3.);
    basic_cmatrix<TP,TPC> mr2 = m2 * ms * alpha + m * beta;
    EXPECT_NEAR(TP(0.), (mr2 - m.hemm (false, ms, m2, alpha, beta)).norm(), sf<TP>()) << "cmatrix::hemm";
}

TYPED_TEST(LapackTest, TestGerReal) {
    TP alpha = 1.3;
    basic_rmatrix<TP> m(3, 4);
    basic_rvector<TP> vc(3);
    basic_rvector<TP> vr(4);
    m.randomize(-1., 2.);
    vc.randomize(-1., 3.);
    vr.randomize(0., 2.);
    basic_rmatrix<TP> mr = m + vc.rank1update (vr) * alpha;
    EXPECT_NEAR(TP(0.), (mr - m.ger(alpha, vc, vr)).norm(), sf<TP>()) << "rmatrix::ger";
}

TYPED_TEST(LapackTest, TestGeruComplex) {
    TPC alpha = TPC(1.2,4.11);
    basic_cmatrix<TP,TPC> m(3, 2);
    basic_cvector<TP,TPC> vc(3);
    basic_cvector<TP,TPC> vr(2);
    m.randomize_real(-1., 2.);
    vc.randomize_real(-1., 3.);
    vr.randomize_real(0., 2.);
    m.randomize_imag(-3., 2.);
    vc.randomize_imag(1., 3.);
    vr.randomize_imag(-1., 2.);
    basic_cmatrix<TP,TPC> mr = m + vc.rank1update_u(vr) * alpha;
    EXPECT_NEAR(TP(0.), (mr - m.geru(alpha, vc, vr)).norm(), sf<TP>()) << "cmatrix::geru";
}

TYPED_TEST(LapackTest, TestGercComplex) {
    TPC alpha = TPC(1.2,4.11);
    basic_cmatrix<TP,TPC> m(3, 2);
    basic_cvector<TP,TPC> vc(3);
    basic_cvector<TP,TPC> vr(2);
    m.randomize_real(-1., 2.);
    vc.randomize_real(-1., 3.);
    vr.randomize_real(0., 2.);
    m.randomize_imag(-3., 2.);
    vc.randomize_imag(1., 3.);
    vr.randomize_imag(-1., 2.);
    basic_cmatrix<TP,TPC> mr = m + vc.rank1update_c(vr) * alpha;
    EXPECT_NEAR(TP(0.), (mr - m.gerc(alpha, vc, vr)).norm(), sf<TP>()) << "cmatrix::gerc";
}

