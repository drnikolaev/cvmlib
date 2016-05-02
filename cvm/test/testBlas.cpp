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
    const basic_rvector<TP> vr{a, 6};
    const basic_cvector<TP,TPC> vc{(TPC*) a, 3};
    EXPECT_EQ(TP{21.}, vr.norm1()) << "rvector::norm1";
    EXPECT_NEAR(15.04631765340644,vc.norm1(),sf<TP>()) << "cvector::norm1";
}

TYPED_TEST(BlasTest, TestVectorNorminf) {
    basic_srsmatrix<TP> ssm1(4);
    basic_srsmatrix<TP> ssm2(4);
    
    basic_rmatrix<TP> m1(4, 4);
    basic_rmatrix<TP> m2(4, 4);
    basic_rmatrix<TP> m3(4, 4);
    
    ssm1.set(1.);
    m1.set(1.);
    
    m2 = ssm1 * m1;
    EXPECT_EQ(TP(16.), m2.norminf()) << "srsmatrix * rmatrix";
    
    m2 = m1 + ssm1;
    EXPECT_EQ(TP(2.), m2(CVM0+2, CVM0+3)) << "srsmatrix + rmatrix";
    
    m2 = m1 * ssm1;
    EXPECT_EQ(TP(16.), m2.norminf()) << "rmatrix * srsmatrix";
}

TYPED_TEST(BlasTest, TestMatrixSumReal) {
    TP a[] = {1., 2., 3., 4., 5., 6.};
    const basic_rmatrix<TP> m1{a, 2, 3};
    basic_rmatrix<TP> m2{2, 3};
    basic_rmatrix<TP> m{2, 3};
    m2.set(1.);
    
    EXPECT_EQ(TP(5.), m.sum(m1,m2)(CVM0+1,CVM0+1)) << "rmatrix::sum";
    EXPECT_EQ(TP(7.), m.sum(m,m2)(CVM0,CVM0+2)) << "rmatrix::sum";
}

TYPED_TEST(BlasTest, TestMatrixDiffReal) {
    TP a[] = {1., 2., 3., 4., 5., 6.};
    const basic_rmatrix<TP> m1{a, 2, 3};
    basic_rmatrix<TP> m2{2, 3};
    basic_rmatrix<TP> m{2, 3};
    m2.set(1.);
    
    EXPECT_EQ(TP(3.), m.diff(m1,m2)(CVM0+1,CVM0+1)) << "rmatrix::diff";
    EXPECT_EQ(TP(3.), m.diff(m,m2)(CVM0,CVM0+2)) << "rmatrix::diff";
}

TYPED_TEST(BlasTest, TestMatrixSumComplex) {
    TP a[] = {1., 2., 3., 4., 5., 6.,
        7., 8., 9., 10., 11., 12.};
    const basic_cmatrix<TP,TPC> ma {(TPC*) a, 2, 3};
    basic_cmatrix<TP,TPC> mb {2, 3};
    basic_cmatrix<TP,TPC> m {2, 3};
    mb.set (TPC(1.,1.));
    
    EXPECT_EQ(TPC(8.,9.),m.sum(ma, mb)(CVM0+1,CVM0+1)) << "cmatrix::sum";
    EXPECT_EQ(TPC(11.,12.),m.sum(m, mb)(CVM0,CVM0+2)) << "cmatrix::sum";
}

TYPED_TEST(BlasTest, TestMatrixDiffComplex) {
    TP a[] = {1., 2., 3., 4., 5., 6.,
        7., 8., 9., 10., 11., 12.};
    const basic_cmatrix<TP,TPC> ma {(TPC*) a, 2, 3};
    basic_cmatrix<TP,TPC> mb {2, 3};
    basic_cmatrix<TP,TPC> m {2, 3};
    mb.set (TPC(1.,1.));
    
    EXPECT_EQ(TPC(6.,7.),m.diff(ma, mb)(CVM0+1,CVM0+1)) << "cmatrix::diff";
    EXPECT_EQ(TPC(7.,8.),m.diff(m, mb)(CVM0,CVM0+2)) << "cmatrix::diff";
}

TYPED_TEST(BlasTest, TestBandMatrixSumReal) {
    TP a[] = {1., 2., 3., 4., 5., 6.};
    const basic_srbmatrix<TP> m1{a, 3, 1, 0};
    basic_srbmatrix<TP> m2{3, 1, 0};
    basic_srbmatrix<TP> m{3, 1, 0};
    m2.set(1.);
    EXPECT_EQ(TP(4.), m.sum(m1,m2)(CVM0+1,CVM0+1)) << "srbmatrix::sum";
    EXPECT_EQ(TP(0.), m(CVM0+1,CVM0+2)) << "srbmatrix::sum";
    EXPECT_EQ(TP(3.), m(CVM0+1,CVM0)) << "srbmatrix::sum";
    EXPECT_EQ(TP(4.), m.sum(m,m2)(CVM0+1,CVM0)) << "srbmatrix::sum";
    EXPECT_EQ(TP(7.), m(CVM0+2,CVM0+2)) << "srbmatrix::sum";
}

TYPED_TEST(BlasTest, TestBandMatrixDiffReal) {
    TP a[] = {1., 2., 3., 4., 5., 6.};
    const basic_srbmatrix<TP> m1{a, 3, 1, 0};
    basic_srbmatrix<TP> m2{3, 1, 0};
    basic_srbmatrix<TP> m{3, 1, 0};
    m2.set(1.);
    EXPECT_EQ(TP(2.), m.diff(m1,m2)(CVM0+1,CVM0+1)) << "srbmatrix::diff";
    EXPECT_EQ(TP(0.), m(CVM0+1,CVM0+2)) << "srbmatrix::diff";
    EXPECT_EQ(TP(1.), m(CVM0+1,CVM0)) << "srbmatrix::diff";
    EXPECT_EQ(TP(0.), m.diff(m,m2)(CVM0+1,CVM0)) << "srbmatrix::diff";
    EXPECT_EQ(TP(3.), m(CVM0+2,CVM0+2)) << "srbmatrix::diff";
}

TYPED_TEST(BlasTest, TestBandMatrixSumComplex) {
    TP a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12.};
    const basic_scbmatrix<TP,TPC> m1((TPC*)a, 3, 1, 0);
    basic_scbmatrix<TP,TPC> m2{3, 1, 0};
    basic_scbmatrix<TP,TPC> m{3, 1, 0};
    m2.set(TPC(1.,1.));
    EXPECT_EQ(TPC(4.,5.),m.sum(m1, m2)(CVM0+1,CVM0)) << "scbmatrix::sum";
    EXPECT_EQ(TPC(10.,11.),m(CVM0+2,CVM0+2)) << "scbmatrix::sum";
    EXPECT_EQ(TPC(0.,0.),m(CVM0+1,CVM0+2)) << "scbmatrix::sum";
    
    EXPECT_EQ(TPC(5.,6.),m.sum(m, m2)(CVM0+1,CVM0)) << "scbmatrix::sum";
    EXPECT_EQ(TPC(11.,12.),m(CVM0+2,CVM0+2)) << "scbmatrix::sum";
    EXPECT_EQ(TPC(0.,0.),m(CVM0+1,CVM0+2)) << "scbmatrix::sum";
}

TYPED_TEST(BlasTest, TestBandMatrixDiffComplex) {
    TP a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12.};
    const basic_scbmatrix<TP,TPC> m1((TPC*)a, 3, 1, 0);
    basic_scbmatrix<TP,TPC> m2{3, 1, 0};
    basic_scbmatrix<TP,TPC> m{3, 1, 0};
    m2.set(TPC(1.,1.));
    EXPECT_EQ(TPC(2.,3.),m.diff(m1, m2)(CVM0+1,CVM0)) << "scbmatrix::diff";
    EXPECT_EQ(TPC(8.,9.),m(CVM0+2,CVM0+2)) << "scbmatrix::diff";
    EXPECT_EQ(TPC(0.,0.),m(CVM0+1,CVM0+2)) << "scbmatrix::diff";
    EXPECT_EQ(TPC(1.,2.),m.diff(m, m2)(CVM0+1,CVM0)) << "scbmatrix::diff";
    EXPECT_EQ(TPC(7.,8.),m(CVM0+2,CVM0+2)) << "scbmatrix::diff";
    EXPECT_EQ(TPC(0.,0.),m(CVM0+1,CVM0+2)) << "scbmatrix::diff";
}

TYPED_TEST(BlasTest, TestSymmetricMatrixSumReal) {
    TP a[] = {1., 2., 3., 2., 5., 6., 3., 6., 9.};
    const basic_srsmatrix<TP> m1{a, 3};
    basic_srsmatrix<TP> m2(3);
    basic_srsmatrix<TP> m(3);
    m2.set(1.);
    EXPECT_EQ(TP(6.), m.sum(m1,m2)(CVM0+1,CVM0+1)) << "srsmatrix::sum";
    EXPECT_EQ(TP(4.), m(CVM0,CVM0+2)) << "srsmatrix::sum";
    EXPECT_EQ(TP(4.), m(CVM0+2,CVM0)) << "srsmatrix::sum";
    EXPECT_EQ(TP(7.), m.sum(m,m2)(CVM0+1,CVM0+1)) << "srsmatrix::sum";
    EXPECT_EQ(TP(5.), m(CVM0,CVM0+2)) << "srsmatrix::sum";
    EXPECT_EQ(TP(5.), m(CVM0+2,CVM0)) << "srsmatrix::sum";
}

TYPED_TEST(BlasTest, TestSymmetricMatrixDiffReal) {
    TP a[] = {1., 2., 3., 2., 5., 6., 3., 6., 9.};
    const basic_srsmatrix<TP> m1(a, 3);
    basic_srsmatrix<TP> m2(3);
    basic_srsmatrix<TP> m(3);
    m2.set(1.);
    EXPECT_EQ(TP(4.), m.diff(m1,m2)(CVM0+1,CVM0+1)) << "srsmatrix::diff";
    EXPECT_EQ(TP(2.), m(CVM0,CVM0+2)) << "srsmatrix::diff";
    EXPECT_EQ(TP(2.), m(CVM0+2,CVM0)) << "srsmatrix::diff";
    EXPECT_EQ(TP(3.), m.diff(m,m2)(CVM0+1,CVM0+1)) << "srsmatrix::diff";
    EXPECT_EQ(TP(1.), m(CVM0,CVM0+2)) << "srsmatrix::diff";
    EXPECT_EQ(TP(1.), m(CVM0+2,CVM0)) << "srsmatrix::diff";
}

TYPED_TEST(BlasTest, TestHermitianMatrixSumComplex) {
    TP a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
              0., 3., -1., -2., 0., -3., 3., 0.};
    TP b[] = {1., 0., 1., 1., 1., 1., 1., -1., 1., 0.,
              1., 1., 1., -1., 1., -1., 1., 0.};
    basic_schmatrix<TP,TPC> m1{(TPC*)a, 3};
    basic_schmatrix<TP,TPC> m2{(TPC*)b, 3};
    basic_schmatrix<TP,TPC> m{3};
    EXPECT_EQ(TPC(3.,2.),m.sum(m1, m2)(CVM0+1,CVM0)) << "schmatrix::sum";
    EXPECT_EQ(TPC(4.,0.),m(CVM0+2,CVM0+2)) << "schmatrix::sum";
    EXPECT_EQ(TPC(1.,-4.),m(CVM0+1,CVM0+2)) << "schmatrix::sum";
    EXPECT_EQ(TPC(1.,4.),m(CVM0+2,CVM0+1)) << "schmatrix::sum";
}

TYPED_TEST(BlasTest, TestHermitianMatrixDiffComplex) {
    TP a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
              0., 3., -1., -2., 0., -3., 3., 0.};
    TP b[] = {1., 0., 1., 1., 1., 1., 1., -1., 1., 0.,
              1., 1., 1., -1., 1., -1., 1., 0.};
    basic_schmatrix<TP,TPC> m1{(TPC*)a, 3};
    basic_schmatrix<TP,TPC> m2{(TPC*)b, 3};
    basic_schmatrix<TP,TPC> m{3};
    EXPECT_EQ(TPC(1.,0.),m.diff(m1, m2)(CVM0+1,CVM0)) << "schmatrix::diff";
    EXPECT_EQ(TPC(2.,0.),m(CVM0+2,CVM0+2)) << "schmatrix::diff";
    EXPECT_EQ(TPC(-1.,-2.),m(CVM0+1,CVM0+2)) << "schmatrix::diff";
    EXPECT_EQ(TPC(-1.,2.),m(CVM0+2,CVM0+1)) << "schmatrix::diff";
}

TYPED_TEST(BlasTest, TestMatrixGemvReal) {
    TP alpha = 1.3;
    TP beta = -0.7;
    basic_rmatrix<TP> m(4, 3);
    basic_rvector<TP> c(4);
    basic_rvector<TP> v(3);
    m.randomize(-1., 2.); v.randomize(-1., 3.); c.randomize(0., 2.);
    basic_rvector<TP> vr1 = m * v * alpha + c * beta;
    EXPECT_NEAR(TP(0.), (vr1 - c.gemv(false, m, alpha, v, beta)).norm(), sf<TP>()) << "rvector::gemv";
    basic_rvector<TP> vr2 = c * m * alpha + v * beta;
    EXPECT_NEAR(TP(0.), (vr2 - v.gemv(true, m, alpha, c, beta)).norm(), sf<TP>()) << "rvector::gemv";
}

TYPED_TEST(BlasTest, TestBandMatrixGbmvReal) {
    TP alpha = 1.3;
    TP beta = -0.7;
    basic_srbmatrix<TP> m(3, 1, 0);
    basic_rvector<TP> c(3);
    basic_rvector<TP> v(3);
    m.randomize(-1., 2.); v.randomize(-1., 3.); c.randomize(0., 2.);
    basic_rvector<TP> vr1 = m * v * alpha + c * beta;
    EXPECT_NEAR(TP(0.), (vr1 - c.gbmv(false, m, alpha, v, beta)).norm(), sf<TP>()) << "rvector::gbmv";
    basic_rvector<TP> vr2 = c * m * alpha + v * beta;
    EXPECT_NEAR(TP(0.), (vr2 - v.gbmv(true, m, alpha, c, beta)).norm(), sf<TP>()) << "rvector::gbmv";
}

TYPED_TEST(BlasTest, TestMatrixGemvComplex) {
    TPC alpha = TPC(1.3,-0.7);
    TPC beta  = TPC(0.15,-1.09);
    basic_cmatrix<TP,TPC> m(3, 2);
    basic_cvector<TP,TPC> c(3);
    basic_cvector<TP,TPC> v(2);
    m.randomize_real(-1., 2.);
    m.randomize_imag(0., 1.);
    v.randomize_real(-1., 3.);
    v.randomize_imag(2., 4.);
    c.randomize_real(0., 2.);
    c.randomize_imag(3., 7.);
    basic_cvector<TP,TPC> vr1 = m * v * alpha + c * beta;
    EXPECT_NEAR(TP(0.), (vr1 - c.gemv(false, m, alpha, v, beta)).norm(), sf<TP>()) << "cvector::gemv";
    basic_cvector<TP,TPC> vr2 = c * m * alpha + v * beta;
    EXPECT_NEAR(TP(0.), (vr2 - v.gemv(true, m, alpha, c, beta)).norm(), sf<TP>()) << "cvector::gemv";
}

TYPED_TEST(BlasTest, TestBandMatrixGbmvComplex) {
    TPC alpha = TPC(1.3,-0.7);
    TPC beta  = TPC(0.15,-1.09);
    basic_scbmatrix<TP,TPC> m(3, 1, 0);
    basic_cvector<TP,TPC> c(3);
    basic_cvector<TP,TPC> v(3);
    m.randomize_real(-1., 2.);
    m.randomize_imag(0., 1.);
    v.randomize_real(-1., 3.);
    v.randomize_imag(2., 4.);
    c.randomize_real(0., 2.);
    c.randomize_imag(3., 7.);
    basic_cvector<TP,TPC> vr1 = m * v * alpha + c * beta;
    EXPECT_NEAR(TP(0.), (vr1 - c.gbmv(false, m, alpha, v, beta)).norm(), sf<TP>()) << "cvector::gbmv";
    basic_cvector<TP,TPC> vr2 = c * m * alpha + v * beta;
    EXPECT_NEAR(TP(0.), (vr2 - v.gbmv(true, m, alpha, c, beta)).norm(), sf<TP>()) << "cvector::gbmv";
}


TYPED_TEST(BlasTest, TestSymmetricMatrixPolynom1) {
    TP a[] = {1., 2., 1., 2., 0., -1., 1., -1., 2.};
    TP av[] = {TP(2.2), TP(1.3), TP(1.1), TP(-0.9), TP(0.2),
        TP(-0.45), TP(45.), TP(-30.), TP(10.), TP(3.), TP(3.2)};
    const basic_rvector<TP> v(av, 11);
    const basic_srsmatrix<TP> m(a, 3);
    const basic_srsmatrix<TP> mp = m.polynom (v);
    EXPECT_NEAR(TP(6.212740000000001e+004), mp(CVM0, CVM0), spp<TP>(1.e-10,4.e-3)) << "srsmatrix::polynom";
    EXPECT_NEAR(TP(2.399800000000000e+004), mp(CVM0+1, CVM0), sp<TP>()) << "srsmatrix::polynom";
    EXPECT_NEAR(TP(3.410055000000000e+004), mp(CVM0+2, CVM0), sp<TP>()) << "srsmatrix::polynom";
    EXPECT_NEAR(TP(2.399800000000000e+004), mp(CVM0, CVM0+1), sp<TP>()) << "srsmatrix::polynom";
    EXPECT_NEAR(TP(2.802685000000000e+004), mp(CVM0+1, CVM0+1), sp<TP>()) << "srsmatrix::polynom";
    EXPECT_NEAR(TP(1.010255000000000e+004), mp(CVM0+2, CVM0+1), spp<TP>(1.e-11,2.e-3)) << "srsmatrix::polynom";
    EXPECT_NEAR(TP(3.410055000000000e+004), mp(CVM0, CVM0+2), sp<TP>()) << "srsmatrix::polynom";
    EXPECT_NEAR(TP(1.010255000000000e+004), mp(CVM0+1, CVM0+2), spp<TP>(1.e-13,2.e-3)) << "srsmatrix::polynom";
    EXPECT_NEAR(TP(5.202485000000000e+004), mp(CVM0+2, CVM0+2), sp<TP>()) << "srsmatrix::polynom";
}

TYPED_TEST(BlasTest, TestHermitianMatrixPolynom1) {
    TP a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
              0., 3., -1., -2., 0., -3., 3., 0.};
    basic_schmatrix<TP,TPC> m{(TPC*)a, 3};
    TP re[] = {TP(2.2), TP(1.3), TP(1.1), TP(-0.9), TP(0.2),
        TP(-0.45), TP(45.), TP(-30.), TP(10.), TP(3.), TP(1.13)};
    const basic_rvector<TP> vr{re, 11};
    basic_schmatrix<TP,TPC> mp{3};
    mp.polynom (m, vr);
    EXPECT_NEAR(std::abs(TPC(1.231954875800000e+008,0.)),
                std::abs(mp(CVM0, CVM0)), s<TP>()) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(1.417932391600000e+008,7.089661958000000e+007)),
                std::abs(mp(CVM0+1, CVM0)), spp<TP>(1.e-14,20)) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(-8.080273845999999e+007,1.616054769200000e+008)),
                std::abs(mp(CVM0+2, CVM0)), spp<TP>(1.e-7,20)) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(1.417932391600000e+008,-7.089661958000000e+007)),
                std::abs(mp(CVM0, CVM0+1)), spp<TP>(1.e-14,20)) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(2.039982260400000e+008,0.)),
                std::abs(mp(CVM0+1, CVM0+1)), spp<TP>(1.e-14,20)) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(0.,2.325020965000000e+008)),
                std::abs(mp(CVM0+2, CVM0+1)), spp<TP>(1.e-14,20)) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(-8.080273845999999e+007,-1.616054769200000e+008)),
                std::abs(mp(CVM0, CVM0+2)), s<TP>()) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(TP(0.), 2.325020965000000e+008)),
                std::abs(mp(CVM0+1, CVM0+2)), spp<TP>()) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(2.649887267400000e+008,0.)),
                std::abs(mp(CVM0+2, CVM0+2)), s<TP>()) << "schmatrix::polynom";
}

TYPED_TEST(BlasTest, TestHermitianMatrixPolynom2) {
    TP a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
              0., 3., -1., -2., 0., -3., 3., 0.};
    basic_schmatrix<TP,TPC> m{(TPC*)a, 3};
    TP re[] = {TP(2.2), TP(1.3), TP(1.1), TP(-0.9), TP(0.2),
        TP(-0.45), TP(45.), TP(-30.), TP(10.), TP(3.), TP(1.13)};
    TP im[] = {0.5, -2, 0, 1, 3, -3., 30., 0., -9., 0., 1.};
    const basic_cvector<TP,TPC> vc{re, im, 11};
    basic_scmatrix<TP,TPC> mp{3};
    mp.polynom (m, vc);
    EXPECT_NEAR(std::abs(TPC(1.231954875800000e+008,6.128500650000000e+007)),
                std::abs(mp(CVM0, CVM0)), s<TP>()) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(1.065249031600000e+008,1.414332915800000e+008)),
                std::abs(mp(CVM0+1, CVM0)), spp<TP>(1.e-14,20)) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(-1.611952344600000e+008,1.214092289200000e+008)),
                std::abs(mp(CVM0+2, CVM0)), spp<TP>()) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(1.770615751600000e+008,-3.599475799999982e+005)),
                std::abs(mp(CVM0, CVM0+1)), s<TP>()) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(2.039982260400000e+008,1.014812545000000e+008)),
                std::abs(mp(CVM0+1, CVM0+1)), s<TP>()) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(-1.156608320000000e+008,2.325020965000000e+008)),
                std::abs(mp(CVM0+2, CVM0+1)), spp<TP>(1.e-14,20)) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(-4.102424600000009e+005,-2.018017249200000e+008)),
                std::abs(mp(CVM0, CVM0+2)), s<TP>()) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(1.156608320000000e+008,-2.325020965000000e+008)),
                std::abs(mp(CVM0+1, CVM0+2)), spp<TP>()) << "schmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(2.649887267400000e+008,1.318216785000000e+008)),
                std::abs(mp(CVM0+2, CVM0+2)), s<TP>()) << "schmatrix::polynom";
}

TYPED_TEST(BlasTest, TestSymmetricMatrixExponent) {
    TP a[] = {1., 2., 1., 2., 0., -1., 1., -1., 2.};
    const basic_srsmatrix<TP> m(a, 3);
    const basic_srsmatrix<TP> me = m.exp();
    EXPECT_NEAR(TP(9.198262499129212e+000), me(CVM0, CVM0), sf<TP>()) << "srsmatrix::exp";
    EXPECT_NEAR(TP(5.558586002658865e+000), me(CVM0+1, CVM0), sf<TP>()) << "srsmatrix::exp";
    EXPECT_NEAR(TP(3.852443363622600e+000), me(CVM0+2, CVM0), sf<TP>()) << "srsmatrix::exp";
    EXPECT_NEAR(TP(5.558586002658862e+000), me(CVM0, CVM0+1), sf<TP>()) << "srsmatrix::exp";
    EXPECT_NEAR(TP(5.345819135506588e+000), me(CVM0+1, CVM0+1), sf<TP>()) << "srsmatrix::exp";
    EXPECT_NEAR(TP(-1.706142639036258e+000), me(CVM0+2, CVM0+1), sf<TP>()) << "srsmatrix::exp";
    EXPECT_NEAR(TP(3.852443363622601e+000), me(CVM0, CVM0+2), sf<TP>()) << "srsmatrix::exp";
    EXPECT_NEAR(TP(-1.706142639036260e+000), me(CVM0+1, CVM0+2), sf<TP>()) << "srsmatrix::exp";
    EXPECT_NEAR(TP(1.090440513816545e+001), me(CVM0+2, CVM0+2), sf<TP>()) << "srsmatrix::exp";
    /*
     Columns 1 through 2
     
     9.198262499129212e+000    5.558586002658862e+000
     5.558586002658865e+000    5.345819135506588e+000
     3.852443363622600e+000   -1.706142639036258e+000
     
     Column 3
     
     3.852443363622601e+000
     -1.706142639036260e+000
     1.090440513816545e+001
     */
}

TYPED_TEST(BlasTest, TestHermitianMatrixExponent) {
    TP a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
              0., 3., -1., -2., 0., -3., 3., 0.};
    basic_schmatrix<TP,TPC> m{(TPC*)a, 3};
    basic_schmatrix<TP,TPC> me{3};
    me.exp(m);
    EXPECT_NEAR(std::abs(TPC(2.673228708371998e+002,0.)),
                std::abs(me(CVM0, CVM0)), sp<TP>()) << "schmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(3.071187567026802e+002,1.535593783513401e+002)),
                std::abs(me(CVM0+1, CVM0)), sp<TP>()) << "schmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-1.749365628720764e+002,3.498731257441527e+002)),
                std::abs(me(CVM0+2, CVM0)), sp<TP>()) << "schmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(3.071187567026802e+002,-1.535593783513401e+002)),
                std::abs(me(CVM0, CVM0+1)), sp<TP>()) << "schmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(4.422594337092769e+002,0.)),
                std::abs(me(CVM0+1, CVM0+1)), sp<TP>()) << "schmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(3.549798266275454e-015,5.034325040954932e+002)),
                std::abs(me(CVM0+2, CVM0+1)), sp<TP>()) << "schmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-1.749365628720763e+002,-3.498731257441526e+002)),
                std::abs(me(CVM0, CVM0+2)), sp<TP>()) << "schmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-1.776065298147746e-014,-5.034325040954931e+002)),
                std::abs(me(CVM0+1, CVM0+2)), sp<TP>()) << "schmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(5.744416275398801e+002,0.)),
                std::abs(me(CVM0+2, CVM0+2)), sp<TP>()) << "schmatrix::exp";
}