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
    EXPECT_NEAR(0,(vX * srm - vB).norm(),spp<TP>()) << "rvector / srmatrix";
    vX = srm % vB;
    EXPECT_NEAR(0,(vX * srm - vB).norm(),spp<TP>()) << "srmatrix % rvector";

    vX = vB / srbm;
    EXPECT_NEAR(0,(vX * srbm - vB).norm(),spp<TP>(1.e-10,0.01)) << "rvector / srbmatrix";
    vX = srbm % vB;
    EXPECT_NEAR(0,(vX * srbm - vB).norm(),spp<TP>(1.e-10,0.01)) << "srbmatrix % rvector";

    vX = vB / srsm;
    EXPECT_NEAR(0,(vX * srsm - vB).norm(),spp<TP>()) << "rvector / srsmatrix";
    vX = srsm % vB;
    EXPECT_NEAR(0,(vX * srsm - vB).norm(),spp<TP>()) << "srsmatrix % rvector";

    vX = vB % srm;
    EXPECT_NEAR(0,(srm * vX - vB).norm(),spp<TP>()) << "rvector % srmatrix";
    vX = srm / vB;
    EXPECT_NEAR(0,(srm * vX - vB).norm(),spp<TP>()) << "srmatrix / rvector";

    vX = vB % srbm;
    EXPECT_NEAR(0,(srbm * vX - vB).norm(),spp<TP>()) << "rvector % srbmatrix";
    vX = srbm / vB;
    EXPECT_NEAR(0,(srbm * vX - vB).norm(),spp<TP>()) << "srbmatrix / rvector";

    vX = vB % srsm;
    EXPECT_NEAR(0,(srsm * vX - vB).norm(),spp<TP>()) << "rvector % srsmatrix";
    vX = srsm / vB;
    EXPECT_NEAR(0,(srsm * vX - vB).norm(),spp<TP>()) << "srsmatrix / rvector";
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
    EXPECT_NEAR(TP(0.), (mh - mh2).norm(), spp<TP>(1.e-12, 0.6)) << "schmatrix::her2k";
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
    EXPECT_NEAR(TP(0.), (mh - mh2).norm(), spp<TP>(1.e-12, 0.5)) << "schmatrix::her2k";
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
    EXPECT_NEAR(TP(0.), (ma * mx1 - mb1).norm(), sp<TP>()) << "cmatrix::solve_lu";
    EXPECT_NEAR(TP(0.), (ma * mx2 - mb2).norm(), sp<TP>()) << "cmatrix::solve_lu";
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
                sf<TP>()) << "cmatrix::gemm";
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

TYPED_TEST(LapackTest, TestPinvReal) {
    // Gantmaher, p. 33
    basic_rmatrix<TP> mA(3, 4);
    mA(CVM0, CVM0) =  1.; mA(CVM0, CVM0+1) = -1.; mA(CVM0, CVM0+2) =  2.; mA(CVM0, CVM0+3) =  0.;
    mA(CVM0+1, CVM0) = -1.; mA(CVM0+1, CVM0+1) =  2.; mA(CVM0+1, CVM0+2) = -3.; mA(CVM0+1, CVM0+3) =  1.;
    mA(CVM0+2, CVM0) =  0.; mA(CVM0+2, CVM0+1) =  1.; mA(CVM0+2, CVM0+2) = -1.; mA(CVM0+2, CVM0+3) =  1.;

    // lower rank case
    basic_rmatrix<TP> mX = mA.pinv(sf<TP>());
    EXPECT_NEAR(TP(0.), (mA * mX * mA - mA).norm2(), sf<TP>()) << "pinv, lower rank, m < n";

    // m > n
    mA.transpose();
    basic_rmatrix<TP> mX2 = mA.pinv(sf<TP>());
    EXPECT_NEAR(TP(0.), (mA * mX2 * mA - mA).norm2(), sf<TP>()) << "pinv, lower rank, m > n";

    // full rank case
    mA.transpose();
    mA(CVM0, CVM0+2) = 4.;
    mX.pinv (mA);
    EXPECT_NEAR(TP(0.), (mA * mX * mA - mA).norm2(), sf<TP>()) << "pinv, full rank, m < n";

    // m > n
    mA.transpose();
    mX2.pinv(mA);
    EXPECT_NEAR(TP(0.), (mA * mX2 * mA - mA).norm2(), sf<TP>()) << "pinv, full rank, m > n";
}

TYPED_TEST(LapackTest, TestPinvComplex) {
    basic_cmatrix<TP,TPC> mA(3, 4), mX(4, 3);
    mA.randomize_real(-2., 11.);
    mA.randomize_imag(-9., 7.);

    mX.pinv (mA);
    EXPECT_NEAR(TP(0.), (mA * mX * mA - mA).norm2(), sp<TP>()) << "complex pinv, m < n";

    // m > n
    mA.conj();
    basic_cmatrix<TP,TPC> mX2 = mA.pinv();
    EXPECT_NEAR(TP(0.), (mA * mX2 * mA - mA).norm2(), sp<TP>()) << "complex pinv, m > n";
}

TYPED_TEST(LapackTest, TestPinvBandReal) {
    basic_srbmatrix<TP> mA (40, 1, 2);
    mA.diag(0).randomize(-1., 1.);
    mA.diag(-1).randomize(-1., 1.);
    mA.diag(1).randomize(-1., 1.);

    basic_rmatrix<TP> mX = mA.pinv(sf<TP>());
    EXPECT_NEAR(TP(0.), (mA * mX * mA - mA).norm2(), spp<TP>()) << "srbmatrix pinv";

    mA.transpose();
    mX.pinv (mA);
    EXPECT_NEAR(TP(0.), (mA * mX * mA - mA).norm2(), spp<TP>()) << "srbmatrix pinv";
}

TYPED_TEST(LapackTest, TestPinvBandComplex) {
    basic_scbmatrix<TP,TPC> mA (40, 1, 2);
    mA.diag(0).randomize_real(-1., 1.);
    mA.diag(-1).randomize_real(-1., 1.);
    mA.diag(1).randomize_real(-1., 1.);
    mA.diag(0).randomize_imag(-1., 2.);
    mA.diag(-1).randomize_imag(-1., 2.);
    mA.diag(1).randomize_imag(-1., 2.);

    basic_scmatrix<TP,TPC> mX = mA.pinv(sf<TP>());
    EXPECT_NEAR(TP(0.), (mA * mX * mA - mA).norm2(), spp<TP>()) << "scbmatrix pinv";

    mA.conj();
    mX.pinv (mA);
    EXPECT_NEAR(TP(0.), (mA * mX * mA - mA).norm2(), spp<TP>()) << "scbmatrix pinv";
}

// 5.4.1
TYPED_TEST(LapackTest, TestSvdReal) {
    TP m[] = {1., -1., 1., 2., -2., 1., 3., -2., 1., 0., -2., 1.};
    basic_rmatrix<TP> mA(m, 4, 3);
    basic_rmatrix<TP> mSigma(4, 3);
    basic_rvector<TP> v(3);
    basic_srmatrix<TP> mU(4), mVH(3);
    v.svd(mA, mU, mVH);
    mSigma.diag(0) = v;
    EXPECT_NEAR(TP(0.), (mA * ~mVH - mU * mSigma).norm(), sf<TP>()) << "rmatrix svd";
    EXPECT_NEAR(TP(0.), (~mA * mU - ~(mSigma * mVH)).norm(), sf<TP>()) << "rmatrix svd";
}

TYPED_TEST(LapackTest, TestSvdComplex) {
    TP m[] = {1., -1., 1., 2., -2., 1., 3., -2., 1., 0., -2., 1.};
    basic_cmatrix<TP,TPC> mA((TPC*) m, 2, 3);
    basic_cmatrix<TP,TPC> mSigma(2, 3);
    basic_rvector<TP> v(2);
    basic_scmatrix<TP,TPC> mU(2), mVH(3);
    v = mA.svd(mU, mVH);
    mSigma.diag(0) = basic_cvector<TP,TPC>(v);
    EXPECT_NEAR(TP(0.), (mA * ~mVH - mU * mSigma).norm(), sf<TP>()) << "cmatrix svd";
    EXPECT_NEAR(TP(0.), (~mA * mU - ~(mSigma * mVH)).norm(), sf<TP>()) << "cmatrix svd";
}

// 5.5 QR
TYPED_TEST(LapackTest, TestQRReal) {
    const TP a[] = {1., 2., 3., 4., 5., 6.};
    const basic_rmatrix<TP> mh(a, 2, 3);
    const basic_rmatrix<TP> mv(a, 3, 2);
    basic_srmatrix<TP> s2(2), s3(3);
    basic_rmatrix<TP> h(2, 3), v(3, 2);

    mh.qr(h, s3);

    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(2) -
    		~basic_rmatrix<TP>(h,CVM0,CVM0,2,2) * basic_rmatrix<TP>(h,CVM0,CVM0,2,2)).norm(),
			sf<TP>()) << "rmatrix QR";
    EXPECT_NEAR(TP(0.), (mh - h * s3).norm(), sf<TP>()) << "rmatrix QR";
    mh.qr(s2, h);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(2) - ~s2 * s2).norm(), sf<TP>()) << "rmatrix QR";
    EXPECT_NEAR(TP(0.), (mh - s2 * h).norm(), sf<TP>()) << "rmatrix QR";
    mv.qr(v, s2);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(2) - ~v * v).norm(), sf<TP>()) << "rmatrix QR";
    EXPECT_NEAR(TP(0.), (mv - v * s2).norm(), sf<TP>()) << "rmatrix QR";
    mv.qr(s3, v);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(3) - ~s3 * s3).norm(), sf<TP>()) << "rmatrix QR";
    EXPECT_NEAR(TP(0.), (mv - s3 * v).norm(), sf<TP>()) << "rmatrix QR";
}

TYPED_TEST(LapackTest, TestQRSquareReal) {
	const TP a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
    const basic_srmatrix<TP> m(a, 3);
    basic_srmatrix<TP> q(3), r(3);
    m.qr(q, r);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(3) - ~q * q).norm(), sf<TP>()) << "srmatrix QR";
    EXPECT_NEAR(TP(0.), (m - q * r).norm(), sf<TP>()) << "srmatrix QR";
}

TYPED_TEST(LapackTest, TestQRSquareReal2) {
    TP a[] = {1., 4., 7., 2., 5., 8., 3., 6., 0.};
    basic_srmatrix<TP> A(a, 3);
    basic_srmatrix<TP> Q(3);
    basic_srmatrix<TP> R(3);
    A.qr(Q, R);
    EXPECT_NEAR(TP(0.), (A - Q * R).norm(), sf<TP>()) << "srmatrix QR";
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(3) - ~Q * Q).norm(), sf<TP>()) << "srmatrix QR - Q";
}

TYPED_TEST(LapackTest, TestQRComplex) {
    const TP ar[] = {1., 2., 3., 4., 5., 6.};
    const TP ai[] = {1., -1., 2., -2., 3., -3.};
    const basic_cmatrix<TP,TPC> mh(ar, ai, 2, 3);
    const basic_cmatrix<TP,TPC> mv(ar, ai, 3, 2);
    basic_scmatrix<TP,TPC> s2(2), s3(3);
    basic_cmatrix<TP,TPC>  h(2, 3), v(3, 2);

    mh.qr(h, s3);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(2) -
    		~basic_cmatrix<TP,TPC>(h,CVM0,CVM0,2,2) * basic_cmatrix<TP,TPC>(h,CVM0,CVM0,2,2)).norm(),
			sf<TP>()) << "cmatrix QR";
    EXPECT_NEAR(TP(0.), (mh - h * s3).norm(), sf<TP>()) << "cmatrix QR";

    mh.qr(s2, h);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(2) - ~s2 * s2).norm(), sf<TP>()) << "cmatrix QR";
    EXPECT_NEAR(TP(0.), (mh - s2 * h).norm(), sf<TP>()) << "cmatrix QR";

    mv.qr(v, s2);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(2) - ~v * v).norm(), sf<TP>()) << "cmatrix QR";
    EXPECT_NEAR(TP(0.), (mv - v * s2).norm(), sf<TP>()) << "cmatrix QR";

    mv.qr(s3, v);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(3) - ~s3 * s3).norm(), sf<TP>()) << "cmatrix QR";
    EXPECT_NEAR(TP(0.), (mv - s3 * v).norm(), sf<TP>()) << "cmatrix QR";
}

TYPED_TEST(LapackTest, TestQRSquareComplex) {
	const TP ar[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
	const TP ai[] = {1., -1., 2., -2., 3., -3., 4., -4., 5.};
    const basic_scmatrix<TP,TPC> m(ar, ai, 3);
    basic_scmatrix<TP,TPC> q(3), r(3);
    m.qr(q, r);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(3) - ~q * q).norm(), sf<TP>()) << "scmatrix QR";
    EXPECT_NEAR(TP(0.), (m - q * r).norm(), sf<TP>()) << "scmatrix QR";
}

TYPED_TEST(LapackTest, TestQRSquareComplex2) {
    const int m = 10;
    basic_scmatrix<TP,TPC> A (m);
    A.randomize_real(-10., 10.);
    A.randomize_imag(-10., 10.);
    basic_scmatrix<TP,TPC> Q(m);
    basic_scmatrix<TP,TPC> R(m);
    A.qr(Q, R);
    EXPECT_NEAR(TP(0.), (Q * R - A).norm(), sp<TP>()) << "scmatrix QR";
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(m) - ~Q * Q).norm(), sf<TP>()) << "scmatrix QR - Q";
}

TYPED_TEST(LapackTest, TestQRBandReal) {
    const int m = 10;
    basic_srbmatrix<TP> A (m, 2, 3);
    A.randomize(-10., 10.);
    basic_srmatrix<TP> Q(m);
    basic_srmatrix<TP> R(m);
    A.qr(Q, R);
    EXPECT_NEAR(TP(0.), (Q * R - A).norm(), sp<TP>()) << "srbmatrix QR";
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(m) - ~Q * Q).norm(), sf<TP>()) << "srbmatrix QR - Q";
}

TYPED_TEST(LapackTest, TestQRBandComplex) {
    const int m = 10;
    basic_scbmatrix<TP,TPC> A (m, 2, 3);
    A.randomize_real(-10., 10.);
    A.randomize_imag(-10., 10.);
    basic_scmatrix<TP,TPC> Q(m);
    basic_scmatrix<TP,TPC> R(m);
    A.qr(Q, R);
    EXPECT_NEAR(TP(0.), (Q * R - A).norm(), sp<TP>()) << "scbmatrix QR";
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(m) - ~Q * Q).norm(), sf<TP>()) << "scbmatrix QR - Q";
}

TYPED_TEST(LapackTest, TestQRSymmetricReal) {
    const int m = 10;
    basic_srsmatrix<TP> A (m);
    A.randomize(-10., 10.);
    basic_srmatrix<TP> Q(m);
    basic_srmatrix<TP> R(m);
    A.qr(Q, R);
    EXPECT_NEAR(TP(0.), (Q * R - A).norm(), sp<TP>()) << "srsmatrix QR";
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(m) - ~Q * Q).norm(), sf<TP>()) << "srsmatrix QR - Q";
}

TYPED_TEST(LapackTest, TestQRHermitianComplex) {
    const int m = 10;
    basic_schmatrix<TP,TPC> A (m);
    A.randomize_real(-10., 10.);
    A.randomize_imag(-10., 10.);
    basic_scmatrix<TP,TPC> Q(m);
    basic_scmatrix<TP,TPC> R(m);
    A.qr(Q, R);
    EXPECT_NEAR(TP(0.), (Q * R - A).norm(), sp<TP>()) << "schmatrix QR";
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(m) - ~Q * Q).norm(), sf<TP>()) << "schmatrix QR - Q";
}

// Case 1: economy mode, A is (m x n) and Q is (m x n) and R is (n x n)
TYPED_TEST(LapackTest, TestQREconomyReal) {
	const int m = 10;
	const int n = 5;
    basic_rmatrix<TP> A(m, n);
    A.randomize(-10.0, 10.0);
    basic_rmatrix<TP> Q(m, n);
    basic_srmatrix<TP> R(n);
    A.qr(Q, R);
    EXPECT_NEAR(TP(0.), (Q * R - A).norm(), sp<TP>()) << "rmatrix QR economy";
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(n) - ~Q * Q).norm(), sf<TP>()) << "rmatrix QR - Q economy";
}

// Case 2: full mode, A is (m x n) and Q is (m x m) and R is (m x n)
TYPED_TEST(LapackTest, TestQRFullReal) {
	const int m = 10;
	const int n = 5;
    basic_rmatrix<TP> A(m, n);
    A.randomize(-10.0, 10.0);
    basic_srmatrix<TP> Q(m);
    basic_rmatrix<TP> R(m, n);
    A.qr(Q, R);
    EXPECT_NEAR(TP(0.), (Q * R - A).norm(), sp<TP>()) << "rmatrix QR full";
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(m) - ~Q * Q).norm(), sf<TP>()) << "rmatrix QR - Q full";
}

// Case 1: economy mode, A is (m x n) and Q is (m x n) and R is (n x n)
TYPED_TEST(LapackTest, TestQREconomyComplex) {
	const int m = 10;
	const int n = 5;
    basic_cmatrix<TP,TPC> A(m, n);
    A.randomize_real(-10.0, 10.0);
    A.randomize_imag(-10.0, 10.0);
    basic_cmatrix<TP,TPC> Q(m, n);
    basic_scmatrix<TP,TPC> R(n);
    A.qr(Q, R);
    EXPECT_NEAR(TP(0.), (Q * R - A).norm(), sp<TP>()) << "cmatrix QR economy";
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(n) - ~Q * Q).norm(), sf<TP>())
    	<< "cmatrix QR - Q economy";
}

// Case 2: full mode, A is (m x n) and Q is (m x m) and R is (m x n)
TYPED_TEST(LapackTest, TestQRFullComplex) {
	const int m = 10;
	const int n = 5;
    basic_cmatrix<TP,TPC> A(m, n);
    A.randomize_real(-10.0, 10.0);
    A.randomize_imag(-10.0, 10.0);
    basic_scmatrix<TP,TPC> Q(m);
    basic_cmatrix<TP,TPC> R(m, n);
    A.qr(Q, R);
    EXPECT_NEAR(TP(0.), (Q * R - A).norm(), sp<TP>()) << "cmatrix QR full";
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(m) - ~Q * Q).norm(), sf<TP>())
    	<< "cmatrix QR - Q full";
}

// 6.0 RQ
TYPED_TEST(LapackTest, TestRQReal) {
    basic_rmatrix<TP> mh(2, 3);
    basic_rmatrix<TP> mv(3, 2);
    mh.randomize(-10., 10.);
    mv.randomize(-10., 10.);
    basic_srmatrix<TP> s2(2), s3(3);
    basic_rmatrix<TP>  h(2, 3), v(3, 2);
    mh.rq(h, s3);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(2) -
    		basic_rmatrix<TP>(s3,CVM0+1,CVM0,2,3) * ~basic_rmatrix<TP>(s3,CVM0+1,CVM0,2,3)).norm(),
			sf<TP>()) << "rmatrix RQ (full mode)";
    EXPECT_NEAR(TP(0.), (mh - h * s3).norm(), sf<TP>()) << "rmatrix RQ (full mode)";
    mh.rq(s2, h);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(2) - h * ~h).norm(), sf<TP>()) << "rmatrix RQ (economy mode)";
    EXPECT_NEAR(TP(0.), (mh - s2 * h).norm(), sf<TP>()) << "rmatrix RQ (economy mode)";
}

TYPED_TEST(LapackTest, TestRQComplex) {
    basic_cmatrix<TP,TPC> mh(2, 3);
    basic_cmatrix<TP,TPC> mv(3, 2);
    mh.randomize_real(-10.0, 10.0);
    mh.randomize_imag(-10.0, 10.0);
    mv.randomize_real(-10.0, 10.0);
    mv.randomize_imag(-10.0, 10.0);
    basic_scmatrix<TP,TPC> s2(2), s3(3);
    basic_cmatrix<TP,TPC>  h(2, 3), v(3, 2);

    mh.rq(h, s3);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(2) -
    		basic_cmatrix<TP,TPC>(s3,CVM0+1,CVM0,2,3) * ~basic_cmatrix<TP,TPC>(s3,CVM0+1,CVM0,2,3)).norm(),
			sf<TP>()) << "cmatrix RQ (full mode)";
    EXPECT_NEAR(TP(0.), (mh - h * s3).norm(), sf<TP>()) << "cmatrix RQ (full mode)";

    mh.rq(s2, h);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(2) - h * ~h).norm(), sf<TP>()) << "cmatrix RQ (economy mode)";
    EXPECT_NEAR(TP(0.), (mh - s2 * h).norm(), sf<TP>()) << "cmatrix RQ (economy mode)";
}

TYPED_TEST(LapackTest, TestRQSquareReal) {
    basic_srmatrix<TP> m(3);
    m.randomize(-100., 100.);
    basic_srmatrix<TP> q(3), r(3);
    m.rq(r, q);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(3) - ~q * q).norm(), sp<TP>()) << "srmatrix RQ";
    EXPECT_NEAR(TP(0.), (m - r * q).norm(), sp<TP>()) << "srmatrix RQ";
}

TYPED_TEST(LapackTest, TestRQSquareComplex) {
    basic_scmatrix<TP,TPC> m(3);
    m.randomize_real(-10.0, 10.0);
    m.randomize_imag(-10.0, 10.0);
    basic_scmatrix<TP,TPC> q(3), r(3);
    m.rq(r, q);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(3) - ~q * q).norm(), sp<TP>()) << "scmatrix RQ";
    EXPECT_NEAR(TP(0.), (m - r * q).norm(), sp<TP>()) << "scmatrix RQ";
}

TYPED_TEST(LapackTest, TestRQBandReal) {
    const int m = 10;
    basic_srbmatrix<TP> A (m, 2, 3);
    A.randomize(-10., 10.);
    basic_srmatrix<TP> Q(m);
    basic_srmatrix<TP> R(m);
    A.rq(R, Q);
    EXPECT_NEAR(TP(0.), (R * Q - A).norm(), sp<TP>()) << "srbmatrix RQ";
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(m) - ~Q * Q).norm(), sp<TP>()) << "srbmatrix RQ - Q";
}

TYPED_TEST(LapackTest, TestRQBandComplex) {
    const int m = 10;
    basic_scbmatrix<TP,TPC> A (m, 2, 3);
    A.randomize_real(-10., 10.);
    A.randomize_imag(-10., 10.);
    basic_scmatrix<TP,TPC> Q(m);
    basic_scmatrix<TP,TPC> R(m);
    A.rq(R, Q);
    EXPECT_NEAR(TP(0.), (R * Q - A).norm(), sp<TP>()) << "scbmatrix RQ";
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(m) - ~Q * Q).norm(), sp<TP>()) << "scbmatrix RQ - Q";
}

TYPED_TEST(LapackTest, TestRQSymmetricReal) {
    const int m = 10;
    basic_srsmatrix<TP> A (m);
    A.randomize(-10., 10.);
    basic_srmatrix<TP> Q(m);
    basic_srmatrix<TP> R(m);
    A.rq(R, Q);
    EXPECT_NEAR(TP(0.), (R * Q - A).norm(), sp<TP>()) << "srsmatrix RQ";
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(m) - ~Q * Q).norm(), sp<TP>()) << "srsmatrix RQ - Q";
}

TYPED_TEST(LapackTest, TestRQHermitianComplex) {
    const int m = 10;
    basic_schmatrix<TP,TPC> A (m);
    A.randomize_real(-10., 10.);
    A.randomize_imag(-10., 10.);
    basic_scmatrix<TP,TPC> Q(m);
    basic_scmatrix<TP,TPC> R(m);
    A.rq(R, Q);
    EXPECT_NEAR(TP(0.), (R * Q - A).norm(), sp<TP>()) << "schmatrix RQ";
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(m) - ~Q * Q).norm(), sp<TP>()) << "schmatrix RQ - Q";
}

// 6.0 LQ
TYPED_TEST(LapackTest, TestLQReal) {
	const TP a[] = {1., 2., 3., 4., 5., 6.};
    const basic_rmatrix<TP> mh(a, 2, 3);
    const basic_rmatrix<TP> mv(a, 3, 2);
    basic_srmatrix<TP> s2(2), s3(3);
    basic_rmatrix<TP> h(2, 3), v(3, 2);

    mh.lq(s2, h);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(2) - h * ~h).norm(), sf<TP>()) << "rmatrix LQ (economy)";
    EXPECT_NEAR(TP(0.), (mh - s2 * h).norm(), sf<TP>()) << "rmatrix LQ (economy)";

    mv.lq(s3, v);

    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(2) -
    		~basic_rmatrix<TP>(v,CVM0,CVM0,2,2) * basic_rmatrix<TP>(v,CVM0,CVM0,2,2)).norm(),
			sf<TP>()) << "rmatrix LQ (economy)";
    EXPECT_NEAR(TP(0.), (mv - s3 * v).norm(), sf<TP>()) << "rmatrix LQ (economy)";

    mh.lq(h, s3);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(3) - s3 * ~s3).norm(), sf<TP>()) << "rmatrix LQ (full)";
    EXPECT_NEAR(TP(0.), (mh - h * s3).norm(), sf<TP>()) << "rmatrix LQ (full)";

    mv.lq(v, s2);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(2) - s2 * ~s2).norm(), sf<TP>()) << "rmatrix LQ (full)";
    EXPECT_NEAR(TP(0.), (mv - v * s2).norm(), sf<TP>()) << "rmatrix LQ (full)";
}

TYPED_TEST(LapackTest, TestLQComplex) {
	const TP ar[] = {1., 2., 3., 4., 5., 6.};
	const TP ai[] = {1., -1., 2., -2., 3., -3.};
    const basic_cmatrix<TP,TPC> mh(ar, ai, 2, 3);
    const basic_cmatrix<TP,TPC> mv(ar, ai, 3, 2);
    basic_scmatrix<TP,TPC> s2(2), s3(3);
    basic_cmatrix<TP,TPC>  h(2, 3), v(3, 2);

    mh.lq(s2, h);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(2) - h * ~h).norm(), sf<TP>()) << "cmatrix LQ (economy)";
    EXPECT_NEAR(TP(0.), (mh - s2 * h).norm(), sf<TP>()) << "cmatrix LQ (economy)";

    mv.lq(s3, v);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(2) -
    		~basic_cmatrix<TP,TPC>(v,CVM0,CVM0,2,2) * basic_cmatrix<TP,TPC>(v,CVM0,CVM0,2,2)).norm(),
			sf<TP>()) << "cmatrix LQ (economy)";
    EXPECT_NEAR(TP(0.), (mv - s3 * v).norm(), sf<TP>()) << "cmatrix LQ (economy)";

    mh.lq(h, s3);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(3) - s3 * ~s3).norm(), sf<TP>()) << "cmatrix LQ (full)";
    EXPECT_NEAR(TP(0.), (mh - h * s3).norm(), sf<TP>()) << "cmatrix LQ (full)";

    mv.lq(v, s2);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(2) - s2 * ~s2).norm(), sf<TP>()) << "cmatrix LQ (full)";
    EXPECT_NEAR(TP(0.), (mv - v * s2).norm(), sf<TP>()) << "cmatrix LQ (full)";
}

TYPED_TEST(LapackTest, TestLQSquareReal) {
	const TP a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
    const basic_srmatrix<TP> m(a, 3);
    basic_srmatrix<TP> l(3), q(3);
    m.lq(l, q);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(3) - ~q * q).norm(), sp<TP>()) << "srmatrix LQ";
    EXPECT_NEAR(TP(0.), (m - l * q).norm(), sp<TP>()) << "srmatrix LQ";
}

TYPED_TEST(LapackTest, TestLQSquareComplex) {
	const TP ar[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
	const TP ai[] = {1., -1., 2., -2., 3., -3., 4., -4., 5.};
    const basic_scmatrix<TP,TPC> m(ar, ai, 3);
    basic_scmatrix<TP,TPC> l(3), q(3);
    m.lq(l, q);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(3) - ~q * q).norm(), sp<TP>()) << "scmatrix LQ";
    EXPECT_NEAR(TP(0.), (m - l * q).norm(), sp<TP>()) << "scmatrix LQ";
}

TYPED_TEST(LapackTest, TestLQBandReal) {
    const int m = 10;
    basic_srbmatrix<TP> A (m, 2, 3);
    A.randomize(-10., 10.);
    basic_srmatrix<TP> L(m);
    basic_srmatrix<TP> Q(m);
    A.lq(L, Q);
    EXPECT_NEAR(TP(0.), (L * Q - A).norm(), sp<TP>()) << "srbmatrix LQ";
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(m) - ~Q * Q).norm(), sp<TP>()) << "srbmatrix LQ - Q";
}

TYPED_TEST(LapackTest, TestLQBandComplex) {
    const int m = 10;
    basic_scbmatrix<TP,TPC> A (m, 2, 3);
    A.randomize_real(-10., 10.);
    A.randomize_imag(-10., 10.);
    basic_scmatrix<TP,TPC> L(m);
    basic_scmatrix<TP,TPC> Q(m);
    A.lq(L, Q);
    EXPECT_NEAR(TP(0.), (L * Q - A).norm(), sp<TP>()) << "scbmatrix LQ";
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(m) - ~Q * Q).norm(), sp<TP>()) << "scbmatrix LQ - Q";
}

TYPED_TEST(LapackTest, TestLQSymmetricReal) {
    const int m = 10;
    basic_srsmatrix<TP> A (m);
    A.randomize(-10., 10.);
    basic_srmatrix<TP> L(m);
    basic_srmatrix<TP> Q(m);
    A.lq(L, Q);
    EXPECT_NEAR(TP(0.), (L * Q - A).norm(), sp<TP>()) << "srsmatrix LQ";
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(m) - ~Q * Q).norm(), sp<TP>()) << "srsmatrix LQ - Q";
}

TYPED_TEST(LapackTest, TestLQHermitianComplex) {
    const int m = 10;
    basic_schmatrix<TP,TPC> A (m);
    A.randomize_real(-10., 10.);
    A.randomize_imag(-10., 10.);
    basic_scmatrix<TP,TPC> L(m);
    basic_scmatrix<TP,TPC> Q(m);
    A.lq(L, Q);
    EXPECT_NEAR(TP(0.), (L * Q - A).norm(), sp<TP>()) << "schmatrix LQ";
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(m) - ~Q * Q).norm(), sp<TP>()) << "schmatrix LQ - Q";
}

// 6.0 QL
TYPED_TEST(LapackTest, TestQLReal) {
	const TP a[] = {1., 2., 3., 4., 5., 6.};
    const basic_rmatrix<TP> mv(a, 3, 2);
    basic_srmatrix<TP> s2(2), s3(3);
    basic_rmatrix<TP> v(3, 2);

    mv.ql(v, s2);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(2) - ~v * v).norm(), sf<TP>()) << "rmatrix QL (economy)";
    EXPECT_NEAR(TP(0.), (mv - v * s2).norm(), sf<TP>()) << "rmatrix QL (economy)";

    mv.ql(s3, v);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(2) -
    		~basic_rmatrix<TP>(s3,CVM0,CVM0+1,3,2) * basic_rmatrix<TP>(s3,CVM0,CVM0+1,3,2)).norm(),
			sf<TP>()) << "rmatrix QL (full)";
    EXPECT_NEAR(TP(0.), (mv - s3 * v).norm(), sf<TP>()) << "rmatrix QL (full)";
}

TYPED_TEST(LapackTest, TestQLComplex) {
	const TP ar[] = {1., 2., 3., 4., 5., 6.};
    const TP ai[] = {1., -1., 2., -2., 3., -3.};
    const basic_cmatrix<TP,TPC> mv(ar, ai, 3, 2);
    basic_scmatrix<TP,TPC> s2(2), s3(3);
    basic_cmatrix<TP,TPC> v(3, 2);

    mv.ql(v, s2);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(2) - ~v * v).norm(), sf<TP>()) << "cmatrix QL (economy)";
    EXPECT_NEAR(TP(0.), (mv - v * s2).norm(), sf<TP>()) << "cmatrix QL (economy)";

    mv.ql(s3, v);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(2) -
    		~basic_cmatrix<TP,TPC>(s3,CVM0,CVM0+1,3,2) * basic_cmatrix<TP,TPC>(s3,CVM0,CVM0+1,3,2)).norm(),
			sf<TP>()) << "cmatrix QL (full)";
    EXPECT_NEAR(TP(0.), (mv - s3 * v).norm(), sf<TP>()) << "cmatrix QL (full)";
}

TYPED_TEST(LapackTest, TestQLSquareReal) {
	const TP a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
    const basic_srmatrix<TP> m(a, 3);
    basic_srmatrix<TP> l(3), q(3);
    m.ql(q, l);
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(3) - ~q * q).norm(), sp<TP>()) << "srmatrix QL";
    EXPECT_NEAR(TP(0.), (m - q * l).norm(), sp<TP>()) << "srmatrix QL";
}

TYPED_TEST(LapackTest, TestQLSquareComplex) {
	const TP ar[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
	const TP ai[] = {1., -1., 2., -2., 3., -3., 4., -4., 5.};
    const basic_scmatrix<TP,TPC> m(ar, ai, 3);
    basic_scmatrix<TP,TPC> l(3), q(3);
    m.ql(q, l);
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(3) - ~q * q).norm(), sp<TP>()) << "scmatrix QL";
    EXPECT_NEAR(TP(0.), (m - q * l).norm(), sp<TP>()) << "scmatrix QL";
}

TYPED_TEST(LapackTest, TestQLBandReal) {
    const int m = 10;
    basic_srbmatrix<TP> A (m, 2, 3);
    A.randomize(-10., 10.);
    basic_srmatrix<TP> L(m);
    basic_srmatrix<TP> Q(m);
    A.ql(Q, L);
    EXPECT_NEAR(TP(0.), (Q * L - A).norm(), sp<TP>()) << "srbmatrix QL";
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(m) - ~Q * Q).norm(), sp<TP>()) << "srbmatrix QL - Q";
}

TYPED_TEST(LapackTest, TestQLBandComplex) {
    const int m = 10;
    basic_scbmatrix<TP,TPC> A (m, 2, 3);
    A.randomize_real(-10., 10.);
    A.randomize_imag(-10., 10.);
    basic_scmatrix<TP,TPC> L(m);
    basic_scmatrix<TP,TPC> Q(m);
    A.ql(Q, L);
    EXPECT_NEAR(TP(0.), (Q * L - A).norm(), sp<TP>()) << "scbmatrix QL";
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(m) - ~Q * Q).norm(), sp<TP>()) << "scbmatrix QL - Q";
}

TYPED_TEST(LapackTest, TestQLSymmetricReal) {
    const int m = 10;
    basic_srsmatrix<TP> A (m);
    A.randomize(-10., 10.);
    basic_srmatrix<TP> L(m);
    basic_srmatrix<TP> Q(m);
    A.ql(Q, L);
    EXPECT_NEAR(TP(0.), (Q * L - A).norm(), sp<TP>()) << "srsmatrix QL";
    EXPECT_NEAR(TP(0.), (basic_eye_real<TP>(m) - ~Q * Q).norm(), sp<TP>()) << "srsmatrix QL - Q";
}

TYPED_TEST(LapackTest, TestQLHermitianComplex) {
    const int m = 10;
    basic_schmatrix<TP,TPC> A (m);
    A.randomize_real(-10., 10.);
    A.randomize_imag(-10., 10.);
    basic_scmatrix<TP,TPC> L(m);
    basic_scmatrix<TP,TPC> Q(m);
    A.ql(Q, L);
    EXPECT_NEAR(TP(0.), (Q * L - A).norm(), sp<TP>()) << "schmatrix QL";
    EXPECT_NEAR(TP(0.), (basic_eye_complex<TP,TPC>(m) - ~Q * Q).norm(), sp<TP>()) << "schmatrix QL - Q";
}

// 6.0 LLS
TYPED_TEST(LapackTest, TestGelsReal) {
    basic_rmatrix<TP> a(7, 5);
    basic_rmatrix<TP> bn(7, 2);
    basic_rmatrix<TP> bt(5, 2);
    basic_rvector<TP> vErr(2);
    a.randomize(-1., 1.);
    bn.randomize(-1., 1.);
    bt.randomize(-1., 1.);

    basic_rmatrix<TP> xn = a.gels(false, bn, vErr);
    EXPECT_NEAR(TP(0.), (a.pinv()*bn - xn).norm(), sp<TP>()) << "gels real nontransp";

    basic_rmatrix<TP> xt = a.gels(true, bt, vErr);
    EXPECT_NEAR(TP(0.), (~a*xt-bt).norm(), sp<TP>()) << "gels real transp";
    EXPECT_NEAR(TP(0.), (~a.pinv()*bt - xt).norm(), sp<TP>()) << "gels real transp";

    basic_rmatrix<TP> xn2(5, 2);
    xn2.gels(false, a, bn, vErr);
    EXPECT_NEAR(TP(0.), (xn-xn2).norm(), sp<TP>()) << "gels real transp";
    basic_rmatrix<TP> xt2(7, 2);
    xt2.gels(true, a, bt, vErr);
    EXPECT_NEAR(TP(0.), (xt-xt2).norm(), sp<TP>()) << "gels real transp";
}

TYPED_TEST(LapackTest, TestGelsReal2) {
    basic_rmatrix<TP> a(5, 7);
    basic_rmatrix<TP> bn(5, 2);
    basic_rmatrix<TP> bt(7, 2);
    basic_rvector<TP> vErr(2);
    a.randomize(-1., 1.);
    bn.randomize(-1., 1.);
    bt.randomize(-1., 1.);

    basic_rmatrix<TP> xn = a.gels(false, bn, vErr);
    EXPECT_NEAR(TP(0.), (a*xn-bn).norm(), sf<TP>()) << "gels real nontransp";
    EXPECT_NEAR(TP(0.), (a.pinv()*bn - xn).norm(), sp<TP>()) << "gels real nontransp";

    basic_rmatrix<TP> xt = a.gels(true, bt, vErr);
    EXPECT_NEAR(TP(0.), (~a.pinv()*bt - xt).norm(), sp<TP>()) << "gels real transp";
}

TYPED_TEST(LapackTest, TestGelsBandReal) {
    basic_srbmatrix<TP> a(5, 1, 0);
    basic_rmatrix<TP> bn(5, 2);
    basic_rmatrix<TP> bt(5, 2);
    basic_rvector<TP> vErr(2);
    a.randomize(-1., 1.);
    bn.randomize(-1., 1.);
    bt.randomize(-1., 1.);

    basic_rmatrix<TP> xn = a.gels(false, bn, vErr);
    EXPECT_NEAR(TP(0.), (a*xn-bn).norm(), spp<TP>()) << "gels real nontransp";
    if (sizeof(TP) > 4) {
        EXPECT_NEAR(TP(0.), (a.pinv()*bn - xn).norm(), spp<TP>(1.e-4,75.)) << "gels real nontransp";
    }
    
    basic_rmatrix<TP> xt = a.gels(true, bt, vErr);
    EXPECT_NEAR(TP(0.), (~a*xt-bt).norm(), spp<TP>(1.e-7,1.e-2)) << "gels real transp";
    if (sizeof(TP) > 4) {
        EXPECT_NEAR(TP(0.), (~a.pinv()*bt - xt).norm(), spp<TP>(1.e-4,75.)) << "gels real transp";
    }
}

TYPED_TEST(LapackTest, TestGelsComplex) {
    basic_cmatrix<TP,TPC> a(7, 5);
    basic_cmatrix<TP,TPC> bn(7, 2);
    basic_cmatrix<TP,TPC> bt(5, 2);
    basic_cvector<TP,TPC> vErr(2);
    a.randomize_real(-1., 1.);
    a.randomize_imag(-1., 1.);
    bn.randomize_real(-1., 1.);
    bn.randomize_imag(-1., 1.);
    bt.randomize_real(-1., 1.);
    bt.randomize_imag(-1., 1.);

    basic_cmatrix<TP,TPC> xn = a.gels(false, bn, vErr);
    EXPECT_NEAR(TP(0.), (a.pinv()*bn - xn).norm(), sf<TP>()) << "gels complex nontransp";

    basic_cmatrix<TP,TPC> xt = a.gels(true, bt, vErr);
    EXPECT_NEAR(TP(0.), (~a*xt-bt).norm(), sf<TP>()) << "gels complex transp";
    EXPECT_NEAR(TP(0.), (~a.pinv()*bt - xt).norm(), sf<TP>()) << "gels complex transp";

    basic_cmatrix<TP,TPC> xn2(5, 2);
    xn2.gels(false, a, bn, vErr);
    EXPECT_NEAR(TP(0.), (xn-xn2).norm(), sf<TP>()) << "gels real nontransp";
    basic_cmatrix<TP,TPC> xt2(7, 2);
    xt2.gels(true, a, bt, vErr);
    EXPECT_NEAR(TP(0.), (xt-xt2).norm(), sf<TP>()) << "gels real transp";
}

TYPED_TEST(LapackTest, TestGelsComplex2) {
    basic_cmatrix<TP,TPC> a(5, 7);
    basic_cmatrix<TP,TPC> bn(5, 2);
    basic_cmatrix<TP,TPC> bt(7, 2);
    basic_cvector<TP,TPC> vErr(2);
    a.randomize_real(-1., 1.);
    a.randomize_imag(-1., 1.);
    bn.randomize_real(-1., 1.);
    bn.randomize_imag(-1., 1.);
    bt.randomize_real(-1., 1.);
    bt.randomize_imag(-1., 1.);

    basic_cmatrix<TP,TPC> xn = a.gels(false, bn, vErr);
    EXPECT_NEAR(TP(0.), (a*xn-bn).norm(), sf<TP>()) << "gels complex nontransp";
    EXPECT_NEAR(TP(0.), (a.pinv()*bn - xn).norm(), sf<TP>()) << "gels complex nontransp";

    basic_cmatrix<TP,TPC> xt = a.gels(true, bt, vErr);
    EXPECT_NEAR(TP(0.), (~a.pinv()*bt - xt).norm(), sf<TP>()) << "gels complex transp";
}

TYPED_TEST(LapackTest, TestGelsBandComplex) {
    basic_scbmatrix<TP,TPC> a(5, 1, 0);
    basic_cmatrix<TP,TPC> bn(5, 2);
    basic_cmatrix<TP,TPC> bt(5, 2);
    basic_cvector<TP,TPC> vErr(2);
    a.randomize_real(-1., 1.);
    a.randomize_imag(-1., 1.);
    bn.randomize_real(-1., 1.);
    bn.randomize_imag(-1., 1.);
    bt.randomize_real(-1., 1.);
    bt.randomize_imag(-1., 1.);

    basic_cmatrix<TP,TPC> xn = a.gels(false, bn, vErr);
    EXPECT_NEAR(TP(0.), (a*xn-bn).norm(), sp<TP>()) << "gels complex nontransp";
    EXPECT_NEAR(TP(0.), (a.pinv()*bn - xn).norm(), spp<TP>()) << "gels complex nontransp";

    basic_cmatrix<TP,TPC> xt = a.gels(true, bt, vErr);
    EXPECT_NEAR(TP(0.), (~a*xt-bt).norm(), sp<TP>()) << "gels complex transp";
    EXPECT_NEAR(TP(0.), (~a.pinv()*bt - xt).norm(), spp<TP>()) << "gels complex transp";
}

// real vector gels*
TYPED_TEST(LapackTest, TestGelsRealVector) {
    basic_rmatrix<TP> a(7, 5);
    basic_rvector<TP> bn(7);
    basic_rvector<TP> bt(5);
    TP dErr;
    a.randomize(-1., 1.);
    bn.randomize(-1., 1.);
    bt.randomize(-1., 1.);

    basic_rvector<TP> xn = a.gels(false, bn, dErr);
    EXPECT_NEAR(TP(0.), (a.pinv()*bn - xn).norm(), sp<TP>()) << "gels real nontransp";

    basic_rvector<TP> xt = a.gels(true, bt, dErr);
    EXPECT_NEAR(TP(0.), (~a*xt-bt).norm(), sp<TP>()) << "gels real transp";
    EXPECT_NEAR(TP(0.), (~a.pinv()*bt - xt).norm(), sp<TP>()) << "gels real transp";

    basic_rvector<TP> xn2(5);
    xn2.gels(false, a, bn, dErr);
    EXPECT_NEAR(TP(0.), (xn-xn2).norm(), sp<TP>()) << "gels real nontransp";
    basic_rvector<TP> xt2(7);
    xt2.gels(true, a, bt, dErr);
    EXPECT_NEAR(TP(0.), (xt-xt2).norm(), sp<TP>()) << "gels real transp";

    basic_rvector<TP> xy(5);
    tint rank;
    xy.gelsy (a, bn, rank);
    EXPECT_NEAR(TP(0.), (xy-xn2).norm(), sp<TP>()) << "gelsy real vector";
    EXPECT_EQ(a.rank(), rank) << "gelsy real vector rank";

    xy = a.gelsy (bn, rank);
    EXPECT_NEAR(TP(0.), (xy-xn2).norm(), sp<TP>()) << "gelsy real vector";
    EXPECT_EQ(a.rank(), rank) << "gelsy real vector rank";

    basic_rvector<TP> sv(5);

    basic_rvector<TP> xs(5);
    xs.gelss (a, bn, sv, rank);
    EXPECT_NEAR(TP(0.), (xy-xs).norm(), sp<TP>()) << "gelss real vector";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss real vector svd";
    EXPECT_EQ(a.rank(), rank) << "gelss real vector rank";

    basic_rvector<TP> xs2 = a.gelss (bn, sv, rank);
    EXPECT_NEAR(TP(0.), (xs-xs2).norm(), sp<TP>()) << "gelss real vector";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss real vector svd";
    EXPECT_EQ(a.rank(), rank) << "gelss real vector rank";

    basic_rvector<TP> xd(5);
    xd.gelsd (a, bn, sv, rank);
    EXPECT_NEAR(TP(0.), (xy-xd).norm(), sp<TP>()) << "gelsd real vector";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelsd real vector svd";
    EXPECT_EQ(a.rank(), rank) << "gelsd real vector rank";

    basic_rvector<TP> xd2 = a.gelsd (bn, sv, rank);
    EXPECT_NEAR(TP(0.), (xd-xd2).norm(), sp<TP>()) << "gelsd real vector";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelsd real vector svd";
    EXPECT_EQ(a.rank(), rank) << "gelsd real vector rank";
}

TYPED_TEST(LapackTest, TestGelsRealVector2) {
    basic_rmatrix<TP> a(5, 7);
    basic_rvector<TP> bn(5);
    basic_rvector<TP> bt(7);
    TP dErr;
    a.randomize(-1., 1.);
    bn.randomize(-1., 1.);
    bt.randomize(-1., 1.);

    basic_rvector<TP> xn = a.gels(false, bn, dErr);
    EXPECT_NEAR(TP(0.), (a*xn-bn).norm(), sf<TP>()) << "gels real nontransp";
    EXPECT_NEAR(TP(0.), (a.pinv()*bn - xn).norm(), sp<TP>()) << "gels real nontransp";

    basic_rvector<TP> xt = a.gels(true, bt, dErr);
    EXPECT_NEAR(TP(0.), (~a.pinv()*bt - xt).norm(), sp<TP>()) << "gels real transp";
}

TYPED_TEST(LapackTest, TestGelsBandRealVector) {
    basic_srbmatrix<TP> a(5, 1, 0);
    basic_rvector<TP> bn(5);
    basic_rvector<TP> bt(5);
    TP dErr;
    a.randomize(-1., 1.);
    bn.randomize(-1., 1.);
    bt.randomize(-1., 1.);

    basic_rvector<TP> xn = a.gels(false, bn, dErr);
    EXPECT_NEAR(TP(0.), (a*xn-bn).norm(), spp<TP>()) << "gels real nontransp";
    if (sizeof(TP) > 4) {
        EXPECT_NEAR(TP(0.), (a.pinv()*bn - xn).norm(), spp<TP>(1.e-5,2.e-1)) << "gels real nontransp";
    }
    
    basic_rvector<TP> xt = a.gels(true, bt, dErr);
    EXPECT_NEAR(TP(0.), (~a*xt-bt).norm(), spp<TP>()) << "gels real transp";
    if (sizeof(TP) > 4) {
        EXPECT_NEAR(TP(0.), (~a.pinv()*bt - xt).norm(), spp<TP>(1.e-5,2.e-1)) << "gels real transp";
    }
}

// complex vector gels*
TYPED_TEST(LapackTest, TestGelsomplexVector) {
    basic_cmatrix<TP,TPC> a(7, 5);
    basic_cvector<TP,TPC> bn(7);
    basic_cvector<TP,TPC> bt(5);
    TPC cErr;
    a.randomize_real(-1., 1.);
    a.randomize_imag(-1., 1.);
    bn.randomize_real(-1., 1.);
    bn.randomize_imag(-1., 1.);
    bt.randomize_real(-1., 1.);
    bt.randomize_imag(-1., 1.);

    basic_cvector<TP,TPC> xn = a.gels(false, bn, cErr);
    EXPECT_NEAR(TP(0.), (a.pinv()*bn - xn).norm(), sp<TP>()) << "gels complex nontransp";

    basic_cvector<TP,TPC> xt = a.gels(true, bt, cErr);
    EXPECT_NEAR(TP(0.), (~a*xt-bt).norm(), sp<TP>()) << "gels complex transp";
    EXPECT_NEAR(TP(0.), (~a.pinv()*bt - xt).norm(), spp<TP>()) << "gels complex transp";

    basic_cvector<TP,TPC> xn2(5);
    xn2.gels(false, a, bn, cErr);
    EXPECT_NEAR(TP(0.), (xn-xn2).norm(), sp<TP>()) << "gels complex nontransp";
    basic_cvector<TP,TPC> xt2(7);
    xt2.gels(true, a, bt, cErr);
    EXPECT_NEAR(TP(0.), (xt-xt2).norm(), sp<TP>()) << "gels complex transp";

    basic_cvector<TP,TPC> xy(5);
    tint rank;
    xy.gelsy (a, bn, rank);
    EXPECT_NEAR(TP(0.), (xy-xn2).norm(), sp<TP>()) << "gelsy complex vector";
    EXPECT_EQ(a.rank(), rank) << "gelsy complex vector rank";

    xy = a.gelsy (bn, rank);
    EXPECT_NEAR(TP(0.), (xy-xn2).norm(), sp<TP>()) << "gelsy complex vector";
    EXPECT_EQ(a.rank(), rank) << "gelsy complex vector rank";

    basic_rvector<TP> sv(5);

    basic_cvector<TP,TPC> xs(5);
    xs.gelss (a, bn, sv, rank);
    EXPECT_NEAR(TP(0.), (xy-xs).norm(), sp<TP>()) << "gelss complex vector";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss complex vector svd";
    EXPECT_EQ(a.rank(), rank) << "gelss complex vector rank";

    basic_cvector<TP,TPC> xd(5);
    xd.gelss (a, bn, sv, rank);
    EXPECT_NEAR(TP(0.), (xy-xd).norm(), sp<TP>()) << "gelsd complex vector";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelsd complex vector svd";
    EXPECT_EQ(a.rank(), rank) << "gelsd complex vector rank";
}

TYPED_TEST(LapackTest, TestGelsomplexVector2) {
    basic_cmatrix<TP,TPC> a(5, 7);
    basic_cvector<TP,TPC> bn(5);
    basic_cvector<TP,TPC> bt(7);
    TPC cErr;
    a.randomize_real(-1., 1.);
    a.randomize_imag(-1., 1.);
    bn.randomize_real(-1., 1.);
    bn.randomize_imag(-1., 1.);
    bt.randomize_real(-1., 1.);
    bt.randomize_imag(-1., 1.);

    basic_cvector<TP,TPC> xn = a.gels(false, bn, cErr);
    EXPECT_NEAR(TP(0.), (a*xn-bn).norm(), sf<TP>()) << "gels complex nontransp";
    EXPECT_NEAR(TP(0.), (a.pinv()*bn - xn).norm(), sp<TP>()) << "gels complex nontransp";

    basic_cvector<TP,TPC> xt = a.gels(true, bt, cErr);
    EXPECT_NEAR(TP(0.), (~a.pinv()*bt - xt).norm(), sp<TP>()) << "gels complex transp";
}

TYPED_TEST(LapackTest, TestGelsBandComplexVector) {
    basic_scbmatrix<TP,TPC> a(5, 1, 0);
    basic_cvector<TP,TPC> bn(5);
    basic_cvector<TP,TPC> bt(5);
    TPC cErr;
    a.randomize_real(-1., 1.);
    a.randomize_imag(-1., 1.);
    bn.randomize_real(-1., 1.);
    bn.randomize_imag(-1., 1.);
    bt.randomize_real(-1., 1.);
    bt.randomize_imag(-1., 1.);

    basic_cvector<TP,TPC> xn = a.gels(false, bn, cErr);
    EXPECT_NEAR(TP(0.), (a*xn-bn).norm(), sp<TP>()) << "gels complex nontransp";
    EXPECT_NEAR(TP(0.), (a.pinv()*bn - xn).norm(), spp<TP>()) << "gels complex nontransp";

    basic_cvector<TP,TPC> xt = a.gels(true, bt, cErr);
    EXPECT_NEAR(TP(0.), (~a*xt-bt).norm(), sp<TP>()) << "gels complex transp";
    EXPECT_NEAR(TP(0.), (~a.pinv()*bt - xt).norm(), spp<TP>()) << "gels complex transp";
}

// 6.0 gelsy
TYPED_TEST(LapackTest, TestGelsyReal) {
    TP ar[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12., 13., 14., 15.};
    basic_rmatrix<TP> a(ar, 3, 5);
    basic_rmatrix<TP> b(ar, 3, 2);
    tint rank;

    basic_rmatrix<TP> x = a.gelsy(b, rank);

    EXPECT_EQ(2, rank) << "gelsy real rank";
    EXPECT_NEAR(TP(0.), (a*x - b).norm(), sf<TP>()) << "gelsy real";
    // see MATLAB's lsqr
    EXPECT_NEAR(TP(0.6), x(CVM0, CVM0), sf<TP>()) << "gelsy real (lsqr)";
    EXPECT_NEAR(TP(0.4), x(CVM0+1, CVM0), sf<TP>()) << "gelsy real (lsqr)";
    EXPECT_NEAR(TP(0.2), x(CVM0+2, CVM0), sf<TP>()) << "gelsy real (lsqr)";
    EXPECT_NEAR(TP(0.), x(CVM0+3, CVM0), sf<TP>()) << "gelsy real (lsqr)";
    EXPECT_NEAR(TP(-0.2), x(CVM0+4, CVM0), sf<TP>()) << "gelsy real (lsqr)";

    basic_rmatrix<TP> x2(5, 2);
    x2.gelsy(a, b, rank);
    EXPECT_NEAR(TP(0.), (x-x2).norm(), sf<TP>()) << "gelsy real";
    EXPECT_EQ(a.rank(), rank) << "gelsy real rank";
}

TYPED_TEST(LapackTest, TestGelsyComplex) {
    basic_cmatrix<TP,TPC> a(3, 5);
    basic_cmatrix<TP,TPC> b(3, 2);
    a.randomize_real(-1., 1.);
    a.randomize_imag(-1., 1.);
    b.randomize_real(-1., 1.);
    b.randomize_imag(-1., 1.);
    tint rank;

    basic_cmatrix<TP,TPC> x = a.gelsy(b, rank);
    EXPECT_EQ(3, rank) << "gelsy complex rank";
    EXPECT_NEAR(TP(0.), (a*x - b).norm(), sf<TP>()) << "gelsy complex";

    basic_cmatrix<TP,TPC> x2(5, 2);
    x2.gelsy(a, b, rank);
    EXPECT_NEAR(TP(0.), (x-x2).norm(), sf<TP>()) << "gelsy complex";
    EXPECT_EQ(a.rank(), rank) << "gelsy complex rank";
}

// 6.0 gelss/gelsd
TYPED_TEST(LapackTest, TestGelssGelsdReal) {
    TP ar[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12., 13., 14., 15.};
    basic_rmatrix<TP> a(ar, 3, 5);
    basic_rmatrix<TP> b(ar, 3, 2);
    basic_rvector<TP> sv(3);
    tint rank;

    basic_rmatrix<TP> x = a.gelss(b, sv, rank);

    EXPECT_EQ(2, rank) << "gelss real rank";
    EXPECT_NEAR(TP(0.), (a*x - b).norm(), sf<TP>()) << "gelss real";
    // see MATLAB's lsqr
    EXPECT_NEAR(TP(0.6), x(CVM0, CVM0), sf<TP>()) << "gelss real (lsqr)";
    EXPECT_NEAR(TP(0.4), x(CVM0+1, CVM0), sf<TP>()) << "gelss real (lsqr)";
    EXPECT_NEAR(TP(0.2), x(CVM0+2, CVM0), sf<TP>()) << "gelss real (lsqr)";
    EXPECT_NEAR(TP(0.), x(CVM0+3, CVM0), sf<TP>()) << "gelss real (lsqr)";
    EXPECT_NEAR(TP(-0.2), x(CVM0+4, CVM0), sf<TP>()) << "gelss real (lsqr)";

    basic_rmatrix<TP> x2(5, 2);
    x2.gelss(a, b, sv, rank);
    EXPECT_NEAR(TP(0.), (x - x2).norm(), sf<TP>()) << "gelss real";
    EXPECT_EQ(2, rank) << "gelss real rank";
    EXPECT_NEAR(TP(0.), (sv - a.svd()).norm(), sf<TP>()) << "gelss real svd";

    basic_rmatrix<TP> xd = a.gelsd(b, sv, rank);
    EXPECT_NEAR(TP(0.), (x - xd).norm(), sf<TP>()) << "gelsd real";
    EXPECT_EQ(2, rank) << "gelsd real rank";
    EXPECT_NEAR(TP(0.), (sv - a.svd()).norm(), sf<TP>()) << "gelsd real svd";

    basic_rmatrix<TP> xd2(5, 2);
    xd2.gelsd(a, b, sv, rank);
    EXPECT_NEAR(TP(0.), (xd - xd2).norm(), sf<TP>()) << "gelsd real";
    EXPECT_EQ(2, rank) << "gelsd real rank";
    EXPECT_NEAR(TP(0.), (sv - a.svd()).norm(), sf<TP>()) << "gelsd real svd";
}

TYPED_TEST(LapackTest, TestGelssGelsdComplex) {
    basic_cmatrix<TP,TPC> a(3, 5);
    basic_cmatrix<TP,TPC> b(3, 2);
    basic_rvector<TP> sv(3);
    a.randomize_real(-1., 1.);
    a.randomize_imag(-1., 1.);
    b.randomize_real(-1., 1.);
    b.randomize_imag(-1., 1.);
    tint rank;

    basic_cmatrix<TP,TPC> x = a.gelss(b, sv, rank);
    EXPECT_EQ(3, rank) << "gelss complex rank";
    EXPECT_NEAR(TP(0.), (a*x - b).norm(), sf<TP>()) << "gelss complex";

    basic_cmatrix<TP,TPC> x2(5, 2);
    x2.gelss(a, b, sv, rank);
    EXPECT_EQ(3, rank) << "gelss complex rank";
    EXPECT_NEAR(TP(0.), (x - x2).norm(), sf<TP>()) << "gelss complex";

    basic_cmatrix<TP,TPC> xd = a.gelsd(b, sv, rank);
    EXPECT_NEAR(TP(0.), (x - xd).norm(), sf<TP>()) << "gelsd complex";
    EXPECT_EQ(3, rank) << "gelsd complex rank";
    EXPECT_NEAR(TP(0.), (sv - a.svd()).norm(), sf<TP>()) << "gelsd complex svd";

    basic_cmatrix<TP,TPC> xd2(5, 2);
    xd2.gelsd(a, b, sv, rank);
    EXPECT_EQ(3, rank) << "gelsd complex rank";
    EXPECT_NEAR(TP(0.), (x - x2).norm(), sf<TP>()) << "gelsd complex";
}

TYPED_TEST(LapackTest, TestGelssGelsdBandReal) {
    basic_srbmatrix<TP> a(5, 1, 0);
    basic_rvector<TP> b(5);
    basic_rmatrix<TP> bm(5, 2);
    a.randomize(-1., 1.);
    b.randomize(-1., 1.);
    bm.randomize(-1., 1.);
    basic_rvector<TP> sv(5);
    tint rank;

    basic_rvector<TP> xs = a.gelss(b, sv, rank);
    EXPECT_NEAR(TP(0.), (a*xs-b).norm(), spp<TP>()) << "gelss real";
    EXPECT_EQ(a.rank(), rank) << "gelss real rank";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss real svd";

    basic_rmatrix<TP> xsm = a.gelss(bm, sv, rank);
    EXPECT_NEAR(TP(0.), (a*xsm-bm).norm(), spp<TP>()) << "gelss real";
    EXPECT_EQ(a.rank(), rank) << "gelss real rank";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss real svd";

    basic_rmatrix<TP> xsm2(5, 2);
    xsm2.gelss(a, bm, sv, rank);
    EXPECT_NEAR(TP(0.), (a*xsm2-bm).norm(), spp<TP>()) << "gelss real";
    EXPECT_EQ(a.rank(), rank) << "gelss real rank";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss real svd";


    basic_rvector<TP> xd = a.gelsd(b, sv, rank);
    EXPECT_NEAR(TP(0.), (a*xd-b).norm(), spp<TP>()) << "gelsd real";
    EXPECT_EQ(a.rank(), rank) << "gelss real rank";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss real svd";

    basic_rmatrix<TP> xdm = a.gelsd(bm, sv, rank);
    EXPECT_NEAR(TP(0.), (a*xdm-bm).norm(), spp<TP>()) << "gelss real";
    EXPECT_EQ(a.rank(), rank) << "gelss real rank";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss real svd";

    basic_rmatrix<TP> xdm2(5, 2);
    xdm2.gelsd(a, bm, sv, rank);
    EXPECT_NEAR(TP(0.), (a*xsm2-bm).norm(), spp<TP>()) << "gelss real";
    EXPECT_EQ(a.rank(), rank) << "gelss real rank";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss real svd";
}

TYPED_TEST(LapackTest, TestGelssGelsdBandComplex) {
    basic_scbmatrix<TP,TPC> a(5, 1, 0);
    basic_cvector<TP,TPC> b(5);
    basic_cmatrix<TP,TPC> bm(5, 2);
    a.randomize_real(-1., 1.);
    a.randomize_imag(-1., 1.);
    b.randomize_real(-1., 1.);
    b.randomize_imag(-1., 1.);
    bm.randomize_real(-1., 1.);
    bm.randomize_imag(-1., 1.);
    basic_rvector<TP> sv(5);
    tint rank;

    basic_cvector<TP,TPC> xs = a.gelss(b, sv, rank);
    EXPECT_NEAR(TP(0.), (a*xs-b).norm(), sp<TP>()) << "gelss complex";
    EXPECT_EQ(a.rank(), rank) << "gelss complex rank";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss complex svd";

    basic_cmatrix<TP,TPC> xsm = a.gelss(bm, sv, rank);
    EXPECT_NEAR(TP(0.), (a*xsm-bm).norm(), sp<TP>()) << "gelss complex";
    EXPECT_EQ(a.rank(), rank) << "gelss complex rank";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss complex svd";

    basic_cmatrix<TP,TPC> xsm2(5, 2);
    xsm2.gelss(a, bm, sv, rank);
    EXPECT_NEAR(TP(0.), (a*xsm2-bm).norm(), sp<TP>()) << "gelss complex";
    EXPECT_EQ(a.rank(), rank) << "gelss complex rank";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss complex svd";


    basic_cvector<TP,TPC> xd = a.gelsd(b, sv, rank);
    EXPECT_NEAR(TP(0.), (a*xd-b).norm(), sp<TP>()) << "gelsd complex";
    EXPECT_EQ(a.rank(), rank) << "gelss complex rank";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss complex svd";

    basic_cmatrix<TP,TPC> xdm = a.gelsd(bm, sv, rank);
    EXPECT_NEAR(TP(0.), (a*xdm-bm).norm(), sp<TP>()) << "gelss complex";
    EXPECT_EQ(a.rank(), rank) << "gelss complex rank";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss complex svd";

    basic_cmatrix<TP,TPC> xdm2(5, 2);
    xdm2.gelsd(a, bm, sv, rank);
    EXPECT_NEAR(TP(0.), (a*xsm2-bm).norm(), sp<TP>()) << "gelss complex";
    EXPECT_EQ(a.rank(), rank) << "gelss complex rank";
    EXPECT_NEAR(TP(0.), (sv-a.svd()).norm(), sp<TP>()) << "gelss complex svd";
}

// 5.5 left eigenvalues
TYPED_TEST(LapackTest, TestLeftEigReal) {
    basic_srmatrix<TP> m(3);
    basic_scmatrix<TP,TPC> e(3);
    basic_scmatrix<TP,TPC> e_(3);
    basic_cvector<TP,TPC> cv(3), cv1(3);
    m(CVM0, CVM0)=0.1; m(CVM0, CVM0+1)=0.5; m(CVM0, CVM0+2)=0.9;
    m(CVM0+1, CVM0)=0.2; m(CVM0+1, CVM0+1)=0.6; m(CVM0+1, CVM0+2)=1.0;
    m(CVM0+2, CVM0)=0.3; m(CVM0+2, CVM0+1)=0.7; m(CVM0+2, CVM0+2)=1.0;

    cv.eig (m, e);
    cv1 = m.eig (e_);
    EXPECT_NEAR(TP(0), (cv - cv1).norm(), sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(TP(0), (basic_scmatrix<TP,TPC>(m) * e(CVM0) - e(CVM0) * cv(CVM0)).norm(), sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(TP(0), (basic_scmatrix<TP,TPC>(m) * e(CVM0+1) - e(CVM0+1) * cv(CVM0+1)).norm(), sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(TP(0), (basic_scmatrix<TP,TPC>(m) * e(CVM0+2) - e(CVM0+2) * cv(CVM0+2)).norm(), sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(TP(0), (basic_scmatrix<TP,TPC>(m) * e_(CVM0) - e_(CVM0) * cv1(CVM0)).norm(), sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(TP(0), (basic_scmatrix<TP,TPC>(m) * e_(CVM0+1) - e_(CVM0+1) * cv1(CVM0+1)).norm(), sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(TP(0), (basic_scmatrix<TP,TPC>(m) * e_(CVM0+2) - e_(CVM0+2) * cv1(CVM0+2)).norm(), sf<TP>()) << "scmatrix eig";

    cv.eig (m, e, false);
    cv1 = m.eig (e_, false);
    EXPECT_NEAR(TP(0), (cv - cv1).norm(), sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(TP(0), (~e(CVM0) * basic_scmatrix<TP,TPC>(m) - ~e(CVM0) * cv(CVM0)).norm(), sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(TP(0), (~e(CVM0+1) * basic_scmatrix<TP,TPC>(m) - ~e(CVM0+1) * cv(CVM0+1)).norm(), sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(TP(0), (~e(CVM0+2) * basic_scmatrix<TP,TPC>(m) - ~e(CVM0+2) * cv(CVM0+2)).norm(), sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(TP(0), (~e_(CVM0) * basic_scmatrix<TP,TPC>(m) - ~e_(CVM0) * cv1(CVM0)).norm(), sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(TP(0), (~e_(CVM0+1) * basic_scmatrix<TP,TPC>(m) - ~e_(CVM0+1) * cv1(CVM0+1)).norm(), sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(TP(0), (~e_(CVM0+2) * basic_scmatrix<TP,TPC>(m) - ~e_(CVM0+2) * cv1(CVM0+2)).norm(), sf<TP>()) << "scmatrix eig, left";
}

TYPED_TEST(LapackTest, TestLeftEigComplex) {
    basic_scmatrix<TP,TPC> m(3);
    basic_scmatrix<TP,TPC> e(3);
    basic_scmatrix<TP,TPC> e_(3);
    basic_cvector<TP,TPC> cv(3), cv1(3);
    m(CVM0, CVM0)=TPC(0.1,0.01); m(CVM0, CVM0+1)=TPC(0.5, 0.05); m(CVM0, CVM0+2)=TPC(0.9, 0.09);
    m(CVM0+1, CVM0)=TPC(0.2,0.02); m(CVM0+1, CVM0+1)=TPC(0.6, 0.06); m(CVM0+1, CVM0+2)=TPC(1.0, 0.1);
    m(CVM0+2, CVM0)=TPC(0.3,0.03); m(CVM0+2, CVM0+1)=TPC(0.7, 0.07); m(CVM0+2, CVM0+2)=TPC(1.0, -1.0);

    cv.eig(m, e);
    cv1 = m.eig(e_);
    EXPECT_NEAR(TP(0), (cv - cv1).norm(), sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(TP(0), (m * e(CVM0) - e(CVM0) * cv(CVM0)).norm(), sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(TP(0), (m * e(CVM0+1) - e(CVM0+1) * cv(CVM0+1)).norm(), sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(TP(0), (m * e(CVM0+2) - e(CVM0+2) * cv(CVM0+2)).norm(), sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(TP(0), (m * e_(CVM0) - e_(CVM0) * cv1(CVM0)).norm(), sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(TP(0), (m * e_(CVM0+1) - e_(CVM0+1) * cv1(CVM0+1)).norm(), sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(TP(0), (m * e_(CVM0+2) - e_(CVM0+2) * cv1(CVM0+2)).norm(), sf<TP>()) << "scmatrix eig";

    cv.eig (m, e, false);
    cv1 = m.eig (e_ , false);
    EXPECT_NEAR(TP(0), (cv - cv1).norm(), sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(TP(0), (~e(CVM0) * m - ~e(CVM0) * cv(CVM0)).norm(), sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(TP(0), (~e(CVM0+1) * m - ~e(CVM0+1) * cv(CVM0+1)).norm(), sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(TP(0), (~e(CVM0+2) * m - ~e(CVM0+2) * cv(CVM0+2)).norm(), sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(TP(0), (~e_(CVM0) * m - ~e_(CVM0) * cv1(CVM0)).norm(), sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(TP(0), (~e_(CVM0+1) * m - ~e_(CVM0+1) * cv1(CVM0+1)).norm(), sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(TP(0), (~e_(CVM0+2) * m - ~e_(CVM0+2) * cv1(CVM0+2)).norm(), sf<TP>()) << "scmatrix eig, left";
}

TYPED_TEST(LapackTest, TestSolveSymmetric) {
    basic_srsmatrix<TP> m(5);
    m.randomize(3., 7.);
    basic_rvector<TP> x(5), b(5);
    b.randomize(-4., 9.);

    x = m.solve (b);
    EXPECT_NEAR(TP(0), (m * x - b).norm(), spp<TP>()) << "srsmatrix solve";
    TP err;
    x = m.solve (b, err);
    EXPECT_NEAR(TP(0), (m * x - b).norm(), spp<TP>()) << "srsmatrix solve";
    EXPECT_NEAR(TP(0), err, spp<TP>()) << "srsmatrix solve";

    basic_rmatrix<TP> mb(5, 6), mx(5, 6);
    mb.randomize(-4., 9.);

    mx = m.solve (mb);
    EXPECT_NEAR(TP(0), (m * mx - mb).norm(), spp<TP>()) << "srsmatrix solve";

    mx = m.solve (mb, err);
    EXPECT_NEAR(TP(0), (m * mx - mb).norm(), spp<TP>()) << "srsmatrix solve";
    EXPECT_NEAR(TP(0), err, spp<TP>()) << "srsmatrix solve";

    basic_srsmatrix<TP> im(m.inv());
    EXPECT_NEAR(TP(0), (im * m - basic_eye_real<TP>(5)).norm(), sp<TP>()) << "srsmatrix inv";
    im.inv(m);
    EXPECT_NEAR(TP(0), (im * m - basic_eye_real<TP>(5)).norm(), sp<TP>()) << "srsmatrix inv";

    basic_rvector<TP> ev(5), ev1(5), ev2(5);
    basic_srmatrix<TP> evect(5);
    ev.eig (m, evect);
    ev1 = m.eig(evect);
    ev2 = m.eig();

    EXPECT_NEAR(TP(0), (ev - ev1).norm(), sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(TP(0), (ev - ev2).norm(), sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(TP(0), (m * evect(CVM0) - evect(CVM0) * ev1(CVM0)).norm(), sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(TP(0), (m * evect(CVM0+1) - evect(CVM0+1) * ev1(CVM0+1)).norm(), sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(TP(0), (m * evect(CVM0+2) - evect(CVM0+2) * ev1(CVM0+2)).norm(), sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(TP(0), (m * evect(CVM0+3) - evect(CVM0+3) * ev1(CVM0+3)).norm(), sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(TP(0), (m * evect(CVM0+4) - evect(CVM0+4) * ev1(CVM0+4)).norm(), sp<TP>()) << "srsmatrix eig";
}

TYPED_TEST(LapackTest, TestSolveHermitian) {
    basic_schmatrix<TP,TPC> hm(5);
    hm.randomize_real(3., 7.);
    hm.randomize_imag(-5., 4.);
    basic_cvector<TP,TPC> x(5), b(5);
    b.randomize_real(-4., 9.);
    b.randomize_imag(-2., 1.);

    x = hm.solve (b);
    EXPECT_NEAR(TP(0), (hm * x - b).norm(), sp<TP>()) << "schmatrix solve";
    TP err;
    x = hm.solve (b, err);
    EXPECT_NEAR(TP(0), (hm * x - b).norm(), sp<TP>()) << "schmatrix solve";
    EXPECT_NEAR(TP(0), err, sp<TP>()) << "schmatrix solve";

    basic_cmatrix<TP,TPC> mb(5, 6), mx(5, 6);
    mb.randomize_real(-4., 9.);
    mb.randomize_imag(-2., 1.);

    mx = hm.solve (mb);
    EXPECT_NEAR(TP(0), (hm * mx - mb).norm(), sp<TP>()) << "schmatrix solve";

    mx = hm.solve (mb, err);
    EXPECT_NEAR(TP(0), (hm * mx - mb).norm(), sp<TP>()) << "schmatrix solve";
    EXPECT_NEAR(TP(0), err, sp<TP>()) << "schmatrix solve";

    basic_schmatrix<TP,TPC> im(hm.inv());
    EXPECT_NEAR(TP(0), (im * hm - basic_eye_complex<TP,TPC>(5)).norm(), sp<TP>()) << "srsmatrix inv";
    im.inv(hm);
    EXPECT_NEAR(TP(0), (im * hm - basic_eye_complex<TP,TPC>(5)).norm(), sp<TP>()) << "srsmatrix inv";

    basic_rvector<TP> ev(5), ev1(5), ev2(5);
    basic_scmatrix<TP,TPC> evect(5);
    ev.eig (hm, evect);
    ev1 = hm.eig(evect);
    ev2 = hm.eig();

    EXPECT_NEAR(TP(0), (ev - ev1).norm(), sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(TP(0), (ev - ev2).norm(), sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(TP(0), (hm * basic_cvector<TP,TPC>(evect(CVM0)) - evect(CVM0) * ev1(CVM0)).norm(), sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(TP(0), (hm * basic_cvector<TP,TPC>(evect(CVM0+1)) - evect(CVM0+1) * ev1(CVM0+1)).norm(), sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(TP(0), (hm * basic_cvector<TP,TPC>(evect(CVM0+2)) - evect(CVM0+2) * ev1(CVM0+2)).norm(), sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(TP(0), (hm * basic_cvector<TP,TPC>(evect(CVM0+3)) - evect(CVM0+3) * ev1(CVM0+3)).norm(), sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(TP(0), (hm * basic_cvector<TP,TPC>(evect(CVM0+4)) - evect(CVM0+4) * ev1(CVM0+4)).norm(), sp<TP>()) << "srsmatrix eig";
}

// positive-definite:
TYPED_TEST(LapackTest, TestDetHermitianPositive) {
/*
mpositive = zeros(3, 3);
m(1, 1) = 1;       m(1, 2) = 2+i;     m(1, 3) = 1-2*i;
m(2, 1) = 2-i;     m(2, 2) = 15;      m(2, 3) = -1-3*i;
m(3, 1) = 1+2*i;   m(3, 2) = -1+3*i;  m(3, 3) = 20;
det (m)
*/
    TP r[] = {1., 2., 1., 2., 15., -1., 1., -1., 20.};
    TP i[] = {0., -1., 2., 1., 0., 3., -2., -3., 0.};
    const basic_schmatrix<TP,TPC> m(r, i, 3);
    basic_scmatrix<TP,TPC> c(3);
    c.cholesky(m);
    EXPECT_NEAR(TP(0), (~c * c - m).norm(), sf<TP>()) << "schmatrix cholesky";
    EXPECT_NEAR(std::abs(TPC(145.,0.)), std::abs(m.det()), sf<TP>()) << "schmatrix det (positive_defiite)";
}

// not positive-definite:
TYPED_TEST(LapackTest, TestDetHermitianNonPositive) {
    TP r[] = {1., 2., 1., 2., 5., -1., 1., -1., 20.};
    TP i[] = {0., -1., 2., 1., 0., 3., -2., -3., 0.};
    const basic_schmatrix<TP,TPC> m(r, i, 3);
    EXPECT_NEAR(std::abs(TPC(-5.,0.)), std::abs(m.det()), sp<TP>()) << "schmatrix det (not positive_defiite)";
}

// 8.1 generalized eigenvalues
TYPED_TEST(LapackTest, TestGenEigReal) {
    basic_srmatrix<TP> a(5);
    basic_srbmatrix<TP> b(5, 2, 1);
    a.randomize(-10., 10.);
    b.randomize(-10., 10.);
    basic_cvector<TP,TPC> alpha(5);
    basic_rvector<TP> beta(5);
    basic_scmatrix<TP,TPC> eigVectLeft(5), eigVectRight(5);

    alpha.geneig(a, b, beta);
    for (int i = 0; i < 5; ++i) {
        if (fabs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_TRUE((basic_scmatrix<TP,TPC>(a) - alpha[CVM0+i] / beta[CVM0+i] *
                basic_scmatrix<TP,TPC>(b)).rank(sp<TP>()) < 5) << "srmatrix geneig";
        }
    }

    alpha.geneig(a, b, beta, eigVectLeft, false);
    for (int i = 0; i < 5; ++i) {
        if (fabs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_TRUE((basic_scmatrix<TP,TPC>(a) - alpha[CVM0+i] / beta[CVM0+i] *
                basic_scmatrix<TP,TPC>(b)).rank(sp<TP>()) < 5) << "srmatrix geneig";
        }
    }
    for (int i = 0; i < 5; ++i) {
        if (fabs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_NEAR(TP(0), (~(eigVectLeft(CVM0+i)) * basic_scmatrix<TP,TPC>(a) -
                (alpha[CVM0+i] / beta[CVM0+i]) * ~(eigVectLeft(CVM0+i)) *
                basic_scmatrix<TP,TPC>(b)).norm(), spp<TP>()) << "srmatrix left geneig";
        }
    }

    alpha.geneig(a, b, beta, eigVectRight);
    for (int i = 0; i < 5; ++i) {
        if (fabs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_TRUE((basic_scmatrix<TP,TPC>(a) - alpha[CVM0+i] / beta[CVM0+i] *
            		basic_scmatrix<TP,TPC>(b)).rank(sp<TP>()) < 5) << "srmatrix geneig";
        }
    }
    for (int i = 0; i < 5; ++i) {
        if (fabs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_NEAR(TP(0), (basic_scmatrix<TP,TPC>(a) * eigVectRight(CVM0+i) -
            		(alpha[CVM0+i] / beta[CVM0+i]) * basic_scmatrix<TP,TPC>(b) * eigVectRight(CVM0+i)).norm(),
					sp<TP>()) << "srmatrix right geneig";
        }
    }

    alpha.geneig(a, b, beta, eigVectLeft, eigVectRight);
    for (int i = 0; i < 5; ++i) {
        if (fabs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_TRUE((basic_scmatrix<TP,TPC>(a) - alpha[CVM0+i] / beta[CVM0+i] *
            		basic_scmatrix<TP,TPC>(b)).rank(sp<TP>()) < 5) << "srmatrix geneig";
        }
    }
    for (int i = 0; i < 5; ++i) {
        if (fabs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_NEAR(TP(0), (~(eigVectLeft(CVM0+i)) * basic_scmatrix<TP,TPC>(a) -
            		(alpha[CVM0+i] / beta[CVM0+i]) * ~(eigVectLeft(CVM0+i)) *
					basic_scmatrix<TP,TPC>(b)).norm(), spp<TP>()) << "srmatrix left geneig";
            EXPECT_NEAR(TP(0), (basic_scmatrix<TP,TPC>(a) * eigVectRight(CVM0+i) -
            		(alpha[CVM0+i] / beta[CVM0+i]) * basic_scmatrix<TP,TPC>(b) *
					eigVectRight(CVM0+i)).norm(), spp<TP>()) << "srmatrix right geneig";
        }
    }
}

TYPED_TEST(LapackTest, TestGenEigComplex) {
    basic_scmatrix<TP,TPC> a(5);
    basic_scbmatrix<TP,TPC> b(5, 2, 1);
    a.randomize_real(-10., 10.);
    a.randomize_imag(-10., 10.);
    b.randomize_real(-10., 10.);
    b.randomize_imag(-10., 10.);
    basic_cvector<TP,TPC> alpha(5);
    basic_cvector<TP,TPC> beta(5);
    basic_scmatrix<TP,TPC> eigVectLeft(5), eigVectRight(5);

    alpha.geneig(a, b, beta);
    for (int i = 0; i < 5; ++i) {
        if (std::abs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_TRUE((a - alpha[CVM0+i] / beta[CVM0+i] * b).rank(sp<TP>()) < 5) << "scmatrix geneig";
        }
    }
    alpha.geneig(a, b, beta, eigVectLeft, false);
    for (int i = 0; i < 5; ++i) {
        if (std::abs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_TRUE((a - alpha[CVM0+i] / beta[CVM0+i] * b).rank(sp<TP>()) < 5) << "scmatrix geneig";
        }
    }
    for (int i = 0; i < 5; ++i) {
        if (std::abs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_NEAR(TP(0), (~(eigVectLeft(CVM0+i)) * a - (alpha[CVM0+i] / beta[CVM0+i]) *
            		~(eigVectLeft(CVM0+i)) * b).norm(), sp<TP>()) << "scmatrix left geneig";
        }
    }

    alpha.geneig(a, b, beta, eigVectRight);
    for (int i = 0; i < 5; ++i) {
        if (std::abs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_TRUE((a - alpha[CVM0+i] / beta[CVM0+i] * b).rank(sp<TP>()) < 5) << "scmatrix geneig";
        }
    }
    for (int i = 0; i < 5; ++i) {
        if (std::abs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_NEAR(TP(0), (a * eigVectRight(CVM0+i) - (alpha[CVM0+i] / beta[CVM0+i]) * b *
            		eigVectRight(CVM0+i)).norm(), sp<TP>()) << "scmatrix right geneig";
        }
    }

    alpha.geneig(a, b, beta, eigVectLeft, eigVectRight);
    for (int i = 0; i < 5; ++i) {
        if (std::abs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_TRUE((a - alpha[CVM0+i] / beta[CVM0+i] * b).rank(sp<TP>()) < 5) << "scmatrix geneig";
        }
    }
    for (int i = 0; i < 5; ++i) {
        if (std::abs(beta[CVM0+i]) > spp<TP>()) {
            EXPECT_NEAR(TP(0), (~(eigVectLeft(CVM0+i)) * a - (alpha[CVM0+i] / beta[CVM0+i]) *
            		~(eigVectLeft(CVM0+i)) * b).norm(), sp<TP>()) << "scmatrix left geneig";
            EXPECT_NEAR(TP(0), (a * eigVectRight(CVM0+i) - (alpha[CVM0+i] / beta[CVM0+i]) * b *
            		eigVectRight(CVM0+i)).norm(), sp<TP>()) << "scmatrix right geneig";
        }
    }
}
