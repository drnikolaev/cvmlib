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
    InitializationTest() {
        for (int i = 0; i < 100; ++i)
        {
            a1[i] = i + 1;
            a2[i] = (i + 1) / 10.;
            a3[i] = (i + 1) * 10.;
            a4[i] = (i + 1) / 100.;
            c1[i] = TC(a1[i], a2[i]);
            c2[i] = TC(a2[i], a4[i]);
        }
    }
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

    rvector64 rv64;
    rmatrix64 rm64;
    srmatrix64 srm64;
    cvector64 cv64;
    cmatrix64 cm64;
    scmatrix64 scm64;
    srbmatrix64 srbm64;
    scbmatrix64 scbm64;
    srsmatrix64 srsm64;
    schmatrix64 schm64;

    T a1[100], a2[100], a3[100], a4[100];
    TC c1[100], c2[100];
    const T cs[18] = {3., 0., 2., 1., -1., 2., 2., -1., 3., 0.,
                      0., 3., -1., -2., 0., -3., 5., 0.};
    const T as[9]  = {1., 2., 1., 2., 5., -1., 1., -1., 20.};
};

TYPED_TEST_CASE(InitializationTest, TestTypes);

TYPED_TEST(InitializationTest, TestSizes) {
    ASSERT_GE(sizeof(TP), 4U);
    ASSERT_LE(sizeof(TP), 8U);
    EXPECT_EQ(0,this->ia.size());
    EXPECT_EQ(0,this->rv.size());
    EXPECT_EQ(0,this->rm.size());
    EXPECT_EQ(0,this->rm.msize());
    EXPECT_EQ(0,this->rm.nsize());
    EXPECT_EQ(0,this->srm.size());
    EXPECT_EQ(0,this->srm.msize());
    EXPECT_EQ(0,this->srm.nsize());
    EXPECT_EQ(0,this->srbm.size());
    EXPECT_EQ(0,this->srbm.msize());
    EXPECT_EQ(0,this->srbm.nsize());
    EXPECT_EQ(0,this->srsm.size());
    EXPECT_EQ(0,this->srsm.msize());
    EXPECT_EQ(0,this->srsm.nsize());
    EXPECT_EQ(0,this->cv.size());
    EXPECT_EQ(0,this->cm.size());
    EXPECT_EQ(0,this->cm.msize());
    EXPECT_EQ(0,this->cm.nsize());
    EXPECT_EQ(0,this->scm.size());
    EXPECT_EQ(0,this->scm.msize());
    EXPECT_EQ(0,this->scm.nsize());
    EXPECT_EQ(0,this->scbm.size());
    EXPECT_EQ(0,this->scbm.msize());
    EXPECT_EQ(0,this->scbm.nsize());
    EXPECT_EQ(0,this->schm.size());
    EXPECT_EQ(0,this->schm.msize());
    EXPECT_EQ(0,this->schm.nsize());
}

#if defined(CVM_USE_INITIALIZER_LISTS)
TYPED_TEST(InitializationTest, TestInitList) {
    basic_rvector<TP> rv = { 1., -2., TP(3.456), TP(99.99) };
    basic_rvector<TP> rv0 = {};
    basic_cvector<TP,TPC> cv = { TPC(1.2, 3.4), TPC(3.4, 5.6), TP(99.99) };
    basic_rvector<TP> cv0 = {};
    EXPECT_EQ(0,rv0.size());
    EXPECT_EQ(0,cv0.size());
    EXPECT_EQ(1.,rv[CVM0]);
    EXPECT_EQ(TP(3.456), rv(CVM0 + 2));
    EXPECT_EQ(TPC(1.2, 3.4),cv(CVM0));
    EXPECT_EQ(TPC(99.99, 0.),cv(CVM0 + 2));
}
#endif

#if defined(CVM_USE_USER_LITERALS)
TYPED_TEST(InitializationTest, TestLiterals) {
    std::complex<double> c = 3.4 + 5.6_i;
    EXPECT_EQ(std::complex<double>(3.4, 5.6),c);

    const cvector64 vc = { 2_i, -2_i, 2.1_i, -2.1_i,
        1 + 2_i, 1.1 + 2_i, 1 + 2.1_i, 1.1 + 2.1_i,
        2_i + 4, 2_i + 4.1, 2.1_i + 4, 2.1_i + 4.1,
        1 - 2_i, 1.1 - 2_i, 1 - 2.1_i, 1.1 - 2.1_i,
        2_i - 4, 2_i - 4.1, 2.1_i - 4, 2.1_i - 4.1 };
    EXPECT_NEAR(vc.norm(), 1.4972641717479251e+01, sp<double>());
}
#endif

TYPED_TEST(InitializationTest, TestMoveReal) {
    basic_rmatrix<TP> rm(4,3);
    rm.set(2.);
    TP ar[] = {1., 2., 3., 4., 5.};
    basic_rvector<TP> a(ar, 3, 2), b(3), c(3), aa(3);
    b[CVM0] = 3;
    c[CVM0] = 4;
    rm[CVM0+1] = a + c;

    aa = a; // copy from sparse
    EXPECT_EQ(1.,aa(CVM0));
    EXPECT_EQ(3.,aa(CVM0+1));
    EXPECT_EQ(5.,aa(CVM0+2));

    EXPECT_EQ(2., rm(CVM0,CVM0));
    EXPECT_EQ(5.,rm(CVM0+1,CVM0));
    EXPECT_EQ(2.,rm(CVM0+2,CVM0));
    EXPECT_EQ(2.,rm(CVM0,CVM0+1));
    EXPECT_EQ(3.,rm(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,rm(CVM0+1,CVM0+2));
    EXPECT_EQ(2.,rm(CVM0+3,CVM0+2));

    basic_rvector<TP> d = a + b + c;
    EXPECT_EQ(8.,d(CVM0));
    EXPECT_EQ(3.,d(CVM0+1));
    EXPECT_EQ(5.,d(CVM0+2));

    basic_rvector<TP> e = a + b;
    EXPECT_EQ(4.,e(CVM0));
    EXPECT_EQ(3.,e(CVM0+1));
    EXPECT_EQ(5.,e(CVM0+2));

    iarray ia(3), ib(3);
    ib[CVM0] = 4;
    ia = std::move(ib);
    EXPECT_EQ(4,ia(CVM0));
    EXPECT_EQ(0,ia(CVM0+1));
    EXPECT_EQ(0,ia(CVM0+2));
//  EXPECT_EQ(ib.size(), 0);

    basic_rvector<TP> f(4);
    {
        basic_rmatrix<TP> rm(4,3);
        rm.set(9.);
        f = rm(CVM0);
        basic_rvector<TP> g = std::move(rm[CVM0+1]);
        EXPECT_EQ(g(CVM0), 9.);
        EXPECT_EQ(g(CVM0+1), 9.);
        EXPECT_EQ(g(CVM0+2), 9.);
    }
    EXPECT_EQ(9.,f(CVM0));
    EXPECT_EQ(9.,f(CVM0+1));
    EXPECT_EQ(9.,f(CVM0+2));
    EXPECT_EQ(9.,f(CVM0+3));
}

TYPED_TEST(InitializationTest, TestMoveComplex) {
    basic_cmatrix<TP,TPC> rm(4,3);
    rm.set(2.);
    TP ar[] = {1., 2., 3., 4., 5., 1., 2., 3., 4., 5., -1., -2.};
    basic_cvector<TP,TPC> a((TPC*)ar, 3, 2), b(3), c(3), aa(3);
    b[CVM0] = TPC(3,3);
    c[CVM0] = TPC(4,4);
    rm[CVM0+1] = a + c;

    aa = a; // copy from sparse
    EXPECT_EQ(aa(CVM0),TPC(1.,2.));
    EXPECT_EQ(TPC(5.,1.),aa(CVM0+1));
    EXPECT_EQ(TPC(4.,5.),aa(CVM0+2));

    EXPECT_EQ(TPC(2.,0.),rm(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,6.),rm(CVM0+1,CVM0));
    EXPECT_EQ(TPC(2.,0.),rm(CVM0+2,CVM0));
    EXPECT_EQ(TPC(2.,0.),rm(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,1.),rm(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(4.,5.),rm(CVM0+1,CVM0+2));
    EXPECT_EQ(TPC(2.,0.),rm(CVM0+3,CVM0+2));

    basic_cvector<TP,TPC> e = a + b;
    EXPECT_EQ(TPC(4.,5.),e(CVM0));
    EXPECT_EQ(TPC(5.,1.),e(CVM0+1));
    EXPECT_EQ(TPC(4.,5.),e(CVM0+2));

    basic_cvector<TP,TPC> f(4);
    {
        basic_cmatrix<TP,TPC> cm(4,3);
        cm.set(TPC(2.,3.));
        f = cm(CVM0);

        basic_cvector<TP,TPC> g = std::move(cm[CVM0+1]);
        EXPECT_EQ(g(CVM0),   TPC(2.,3.));
        EXPECT_EQ(g(CVM0+1), TPC(2.,3.));
        EXPECT_EQ(g(CVM0+2), TPC(2.,3.));
    }
    EXPECT_EQ(TPC(2.,3.),f(CVM0));
    EXPECT_EQ(TPC(2.,3.),f(CVM0+1));
    EXPECT_EQ(TPC(2.,3.),f(CVM0+2));
    EXPECT_EQ(TPC(2.,3.),f(CVM0+3));
}

TYPED_TEST(InitializationTest, TestSubmatrixMoveReal) {
    basic_rmatrix<TP> m1(2,3), m2(2,3), m3(2,3);
    m2.set(2.);
    m3.set(3.);
    m1 = m2 + m3;

    EXPECT_EQ(5.,m1(CVM0,CVM0));
    EXPECT_EQ(5.,m1(CVM0+1,CVM0));
    EXPECT_EQ(5.,m1(CVM0,CVM0+1));
    EXPECT_EQ(5.,m1(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,m1(CVM0+1,CVM0+2));

    basic_rmatrix<TP> m4(m2 + m3);
    EXPECT_EQ(5.,m4(CVM0,CVM0));
    EXPECT_EQ(5.,m4(CVM0+1,CVM0));
    EXPECT_EQ(5.,m4(CVM0,CVM0+1));
    EXPECT_EQ(5.,m4(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,m4(CVM0+1,CVM0+2));

    basic_rmatrix<TP> m7(7,5);
    m7.set(7.);
    basic_rmatrix<TP> ms(m7, CVM0+1, CVM0+2, 2, 3); // submatrix
    basic_rmatrix<TP> mm = std::move(ms);

    EXPECT_EQ(7.,mm(CVM0,CVM0));
    EXPECT_EQ(7.,mm(CVM0+1,CVM0));
    EXPECT_EQ(7.,mm(CVM0,CVM0+1));
    EXPECT_EQ(7.,mm(CVM0+1,CVM0+1));
    EXPECT_EQ(7.,mm(CVM0+1,CVM0+2));

    EXPECT_EQ(7.,ms(CVM0,CVM0));
    EXPECT_EQ(7.,ms(CVM0+1,CVM0));
    EXPECT_EQ(7.,ms(CVM0,CVM0+1));
    EXPECT_EQ(7.,ms(CVM0+1,CVM0+1));
    EXPECT_EQ(7.,ms(CVM0+1,CVM0+2));

    ms = m2 + m3;
    EXPECT_EQ(5.,ms(CVM0,CVM0));
    EXPECT_EQ(5.,ms(CVM0+1,CVM0));
    EXPECT_EQ(5.,ms(CVM0,CVM0+1));
    EXPECT_EQ(5.,ms(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,ms(CVM0+1,CVM0+2));

    EXPECT_EQ(7.,m7(CVM0,CVM0+2));
    EXPECT_EQ(5.,m7(CVM0+1,CVM0+2));
    EXPECT_EQ(5.,m7(CVM0+2,CVM0+2));
    EXPECT_EQ(7.,m7(CVM0+3,CVM0+2));
}

TYPED_TEST(InitializationTest, TestSubmatrixMoveComplex) {
    basic_cmatrix<TP,TPC> m1(2,3), m2(2,3), m3(2,3);
    m2.set(TPC(2.,2.));
    m3.set(TPC(3.,3.));
    m1 = m2 + m3;

    EXPECT_EQ(TPC(5.,5.),m1(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,5.),m1(CVM0+1,CVM0));
    EXPECT_EQ(TPC(5.,5.),m1(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),m1(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),m1(CVM0+1,CVM0+2));

    basic_cmatrix<TP,TPC> m4(m2 + m3);
    EXPECT_EQ(TPC(5.,5.),m4(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,5.),m4(CVM0+1,CVM0));
    EXPECT_EQ(TPC(5.,5.),m4(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),m4(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),m4(CVM0+1,CVM0+2));

    basic_cmatrix<TP,TPC> m7(7,5);
    m7.set(TPC(7.,7.));
    basic_cmatrix<TP,TPC> ms(m7, CVM0+1, CVM0+2, 2, 3); // submatrix
    basic_cmatrix<TP,TPC> mm = std::move(ms);

    EXPECT_EQ(TPC(7.,7.),mm(CVM0,CVM0));
    EXPECT_EQ(TPC(7.,7.),mm(CVM0+1,CVM0));
    EXPECT_EQ(TPC(7.,7.),mm(CVM0,CVM0+1));
    EXPECT_EQ(TPC(7.,7.),mm(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(7.,7.),mm(CVM0+1,CVM0+2));

    EXPECT_EQ(TPC(7.,7.),ms(CVM0,CVM0));
    EXPECT_EQ(TPC(7.,7.),ms(CVM0+1,CVM0));
    EXPECT_EQ(TPC(7.,7.),ms(CVM0,CVM0+1));
    EXPECT_EQ(TPC(7.,7.),ms(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(7.,7.),ms(CVM0+1,CVM0+2));

    ms = m2 + m3;
    EXPECT_EQ(TPC(5.,5.),ms(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,5.),ms(CVM0+1,CVM0));
    EXPECT_EQ(TPC(5.,5.),ms(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),ms(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),ms(CVM0+1,CVM0+2));

    EXPECT_EQ(TPC(7.,7.),m7(CVM0,CVM0+2));
    EXPECT_EQ(TPC(5.,5.),m7(CVM0+1,CVM0+2));
    EXPECT_EQ(TPC(5.,5.),m7(CVM0+2,CVM0+2));
    EXPECT_EQ(TPC(7.,7.),m7(CVM0+3,CVM0+2));
}

TYPED_TEST(InitializationTest, TestSquareSubmatrixMoveReal) {
    basic_srmatrix<TP> m1(3), m2(3), m3(3);
    m2.set(2.);
    m3.set(3.);
    m1 = m2 + m3;

    EXPECT_EQ(5.,m1(CVM0,CVM0));
    EXPECT_EQ(5.,m1(CVM0+1,CVM0));
    EXPECT_EQ(5.,m1(CVM0,CVM0+1));
    EXPECT_EQ(5.,m1(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,m1(CVM0+1,CVM0+2));

    basic_srmatrix<TP> m4(m2 + m3);
    EXPECT_EQ(5.,m4(CVM0,CVM0));
    EXPECT_EQ(5.,m4(CVM0+1,CVM0));
    EXPECT_EQ(5.,m4(CVM0,CVM0+1));
    EXPECT_EQ(5.,m4(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,m4(CVM0+1,CVM0+2));

    basic_srmatrix<TP> m7(7);
    m7.set(7.);
    basic_srmatrix<TP> ms(m7, CVM0+1, CVM0+2, 3); // submatrix
    basic_srmatrix<TP> mm = std::move(ms);

    EXPECT_EQ(7.,mm(CVM0,CVM0));
    EXPECT_EQ(7.,mm(CVM0+1,CVM0));
    EXPECT_EQ(7.,mm(CVM0,CVM0+1));
    EXPECT_EQ(7.,mm(CVM0+1,CVM0+1));
    EXPECT_EQ(7.,mm(CVM0+1,CVM0+2));

    EXPECT_EQ(7.,ms(CVM0,CVM0));
    EXPECT_EQ(7.,ms(CVM0+1,CVM0));
    EXPECT_EQ(7.,ms(CVM0,CVM0+1));
    EXPECT_EQ(7.,ms(CVM0+1,CVM0+1));
    EXPECT_EQ(7.,ms(CVM0+1,CVM0+2));

    ms = m2 + m3;
    EXPECT_EQ(5.,ms(CVM0,CVM0));
    EXPECT_EQ(5.,ms(CVM0+1,CVM0));
    EXPECT_EQ(5.,ms(CVM0,CVM0+1));
    EXPECT_EQ(5.,ms(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,ms(CVM0+1,CVM0+2));

    EXPECT_EQ(7.,m7(CVM0,CVM0+2));
    EXPECT_EQ(5.,m7(CVM0+1,CVM0+2));
    EXPECT_EQ(5.,m7(CVM0+2,CVM0+2));
    EXPECT_EQ(5.,m7(CVM0+3,CVM0+2));
    EXPECT_EQ(7.,m7(CVM0+4,CVM0+2));
}

TYPED_TEST(InitializationTest, TestSquareSubmatrixMoveComplex) {
    basic_scmatrix<TP,TPC> m1(3), m2(3), m3(3);
    m2.set(TPC(2.,2.));
    m3.set(TPC(3.,3.));
    m1 = m2 + m3;

    EXPECT_EQ(TPC(5.,5.),m1(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,5.),m1(CVM0+1,CVM0));
    EXPECT_EQ(TPC(5.,5.),m1(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),m1(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),m1(CVM0+1,CVM0+2));

    basic_scmatrix<TP,TPC> m4(m2 + m3);
    EXPECT_EQ(TPC(5.,5.),m4(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,5.),m4(CVM0+1,CVM0));
    EXPECT_EQ(TPC(5.,5.),m4(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),m4(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),m4(CVM0+1,CVM0+2));

    basic_scmatrix<TP,TPC> m7(7);
    m7.set(TPC(7.,7.));
    basic_scmatrix<TP,TPC> ms(m7, CVM0+1, CVM0+2, 3); // submatrix
    basic_scmatrix<TP,TPC> mm = std::move(ms);

    EXPECT_EQ(TPC(7.,7.),mm(CVM0,CVM0));
    EXPECT_EQ(TPC(7.,7.),mm(CVM0+1,CVM0));
    EXPECT_EQ(TPC(7.,7.),mm(CVM0,CVM0+1));
    EXPECT_EQ(TPC(7.,7.),mm(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(7.,7.),mm(CVM0+1,CVM0+2));

    EXPECT_EQ(TPC(7.,7.),ms(CVM0,CVM0));
    EXPECT_EQ(TPC(7.,7.),ms(CVM0+1,CVM0));
    EXPECT_EQ(TPC(7.,7.),ms(CVM0,CVM0+1));
    EXPECT_EQ(TPC(7.,7.),ms(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(7.,7.),ms(CVM0+1,CVM0+2));

    ms = m2 + m3;
    EXPECT_EQ(TPC(5.,5.),ms(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,5.),ms(CVM0+1,CVM0));
    EXPECT_EQ(TPC(5.,5.),ms(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),ms(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),ms(CVM0+1,CVM0+2));

    EXPECT_EQ(TPC(7.,7.),m7(CVM0,CVM0+2));
    EXPECT_EQ(TPC(5.,5.),m7(CVM0+1,CVM0+2));
    EXPECT_EQ(TPC(5.,5.),m7(CVM0+2,CVM0+2));
    EXPECT_EQ(TPC(5.,5.),m7(CVM0+3,CVM0+2));
    EXPECT_EQ(TPC(7.,7.),m7(CVM0+4,CVM0+2));
}

TYPED_TEST(InitializationTest, TestBandMoveReal) {
    basic_srbmatrix<TP> m1(5,2,1), m2(5,2,1), m3(5,2,1);
    m2.set(2.);
    m3.set(3.);
    m1 = m2 + m3;

    EXPECT_EQ(5.,m1(CVM0,CVM0));
    EXPECT_EQ(5.,m1(CVM0+1,CVM0));
    EXPECT_EQ(5.,m1(CVM0,CVM0+1));
    EXPECT_EQ(5.,m1(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,m1(CVM0+1,CVM0+2));

    basic_srbmatrix<TP> m4(m2 + m3);
    EXPECT_EQ(5.,m4(CVM0,CVM0));
    EXPECT_EQ(5.,m4(CVM0+1,CVM0));
    EXPECT_EQ(5.,m4(CVM0,CVM0+1));
    EXPECT_EQ(5.,m4(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,m4(CVM0+1,CVM0+2));

    basic_srbmatrix<TP> mt(~m4);
    EXPECT_EQ(1,mt.lsize());
    EXPECT_EQ(2,mt.usize());

    EXPECT_EQ(5.,mt(CVM0+1,CVM0));
    EXPECT_EQ(0.,mt(CVM0+2,CVM0));
    EXPECT_EQ(5.,mt(CVM0,CVM0+2));
    EXPECT_EQ(0.,mt(CVM0,CVM0+3));

    basic_srmatrix<TP> ms(m1);
    EXPECT_EQ(5.,ms(CVM0,CVM0));
    EXPECT_EQ(5.,ms(CVM0+1,CVM0));
    EXPECT_EQ(5.,ms(CVM0,CVM0+1));
    EXPECT_EQ(5.,ms(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,ms(CVM0+1,CVM0+2));
    EXPECT_EQ(0.,ms(CVM0+1,CVM0+3));
}

TYPED_TEST(InitializationTest, TestBandMoveComplex) {
    basic_scbmatrix<TP,TPC> m1(5,2,1), m2(5,2,1), m3(5,2,1);
    m2.set(TPC(2.,2.));
    m3.set(TPC(3.,3.));
    m1 = m2 + m3;

    EXPECT_EQ(TPC(5.,5.),m1(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,5.),m1(CVM0+1,CVM0));
    EXPECT_EQ(TPC(5.,5.),m1(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),m1(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),m1(CVM0+1,CVM0+2));

    basic_scbmatrix<TP,TPC> m4(m2 + m3);
    EXPECT_EQ(TPC(5.,5.),m4(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,5.),m4(CVM0+1,CVM0));
    EXPECT_EQ(TPC(5.,5.),m4(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),m4(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),m4(CVM0+1,CVM0+2));

    basic_scbmatrix<TP,TPC> mt(~m4);
    EXPECT_EQ(1,mt.lsize());
    EXPECT_EQ(2,mt.usize());

    EXPECT_EQ(TPC(5.,-5.),mt(CVM0+1,CVM0));
    EXPECT_EQ(TPC(0.,0.),mt(CVM0+2,CVM0));
    EXPECT_EQ(TPC(5.,-5.),mt(CVM0,CVM0+2));
    EXPECT_EQ(TPC(0.,0.),mt(CVM0,CVM0+3));

    basic_scmatrix<TP,TPC> ms(m1);
    EXPECT_EQ(TPC(5.,5.),ms(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,5.),ms(CVM0+1,CVM0));
    EXPECT_EQ(TPC(5.,5.),ms(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),ms(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(5.,5.),ms(CVM0+1,CVM0+2));
    EXPECT_EQ(TPC(0.,0.),ms(CVM0+1,CVM0+3));
}

TYPED_TEST(InitializationTest, TestSymmetricSubmatrixMoveReal) {
    basic_srsmatrix<TP> m1(3), m2(3), m3(3);
    m2.set(2.);
    m3.set(3.);
    m1 = m2 + m3;

    EXPECT_EQ(5.,m1(CVM0,CVM0));
    EXPECT_EQ(5.,m1(CVM0+1,CVM0));
    EXPECT_EQ(5.,m1(CVM0,CVM0+1));
    EXPECT_EQ(5.,m1(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,m1(CVM0+1,CVM0+2));

    basic_srsmatrix<TP> m4(m2 + m3);
    EXPECT_EQ(5.,m4(CVM0,CVM0));
    EXPECT_EQ(5.,m4(CVM0+1,CVM0));
    EXPECT_EQ(5.,m4(CVM0,CVM0+1));
    EXPECT_EQ(5.,m4(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,m4(CVM0+1,CVM0+2));

    basic_srsmatrix<TP> m7(7);
    m7.set(7.);
    basic_srsmatrix<TP> ms(m7, CVM0+1, 3); // submatrix
    basic_srsmatrix<TP> mm = std::move(ms);

    EXPECT_EQ(7.,mm(CVM0,CVM0));
    EXPECT_EQ(7.,mm(CVM0+1,CVM0));
    EXPECT_EQ(7.,mm(CVM0,CVM0+1));
    EXPECT_EQ(7.,mm(CVM0+1,CVM0+1));
    EXPECT_EQ(7.,mm(CVM0+1,CVM0+2));

    EXPECT_EQ(7.,ms(CVM0,CVM0));
    EXPECT_EQ(7.,ms(CVM0+1,CVM0));
    EXPECT_EQ(7.,ms(CVM0,CVM0+1));
    EXPECT_EQ(7.,ms(CVM0+1,CVM0+1));
    EXPECT_EQ(7.,ms(CVM0+1,CVM0+2));

    ms = m2 + m3;
    EXPECT_EQ(5.,ms(CVM0,CVM0));
    EXPECT_EQ(5.,ms(CVM0+1,CVM0));
    EXPECT_EQ(5.,ms(CVM0,CVM0+1));
    EXPECT_EQ(5.,ms(CVM0+1,CVM0+1));
    EXPECT_EQ(5.,ms(CVM0+1,CVM0+2));

    EXPECT_EQ(7.,m7(CVM0,CVM0+2));
    EXPECT_EQ(5.,m7(CVM0+1,CVM0+2));
    EXPECT_EQ(5.,m7(CVM0+2,CVM0+2));
    EXPECT_EQ(5.,m7(CVM0+3,CVM0+2));
    EXPECT_EQ(7.,m7(CVM0+4,CVM0+2));
}

TYPED_TEST(InitializationTest, TestHermitianSubmatrixMoveComplex) {
    basic_schmatrix<TP,TPC> m1(3), m2(3), m3(3);
    m2.set_real(2.);
    m3.set_real(3.);
    m1 = m2 + m3;

    EXPECT_EQ(TPC(5.,0.),m1(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,0.),m1(CVM0+1,CVM0));
    EXPECT_EQ(TPC(5.,0.),m1(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,0.),m1(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(5.,0.),m1(CVM0+1,CVM0+2));

    basic_schmatrix<TP,TPC> m4(m2 + m3);
    EXPECT_EQ(TPC(5.,0.),m4(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,0.),m4(CVM0+1,CVM0));
    EXPECT_EQ(TPC(5.,0.),m4(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,0.),m4(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(5.,0.),m4(CVM0+1,CVM0+2));

    basic_schmatrix<TP,TPC> m7(7);
    m7.set_real(7.);
    basic_schmatrix<TP,TPC> ms(m7, CVM0+1, 3); // submatrix
    basic_schmatrix<TP,TPC> mm = std::move(ms);

    EXPECT_EQ(TPC(7.,0.),mm(CVM0,CVM0));
    EXPECT_EQ(TPC(7.,0.),mm(CVM0+1,CVM0));
    EXPECT_EQ(TPC(7.,0.),mm(CVM0,CVM0+1));
    EXPECT_EQ(TPC(7.,0.),mm(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(7.,0.),mm(CVM0+1,CVM0+2));

    EXPECT_EQ(TPC(7.,0.),ms(CVM0,CVM0));
    EXPECT_EQ(TPC(7.,0.),ms(CVM0+1,CVM0));
    EXPECT_EQ(TPC(7.,0.),ms(CVM0,CVM0+1));
    EXPECT_EQ(TPC(7.,0.),ms(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(7.,0.),ms(CVM0+1,CVM0+2));

    ms = m2 + m3;
    EXPECT_EQ(TPC(5.,0.),ms(CVM0,CVM0));
    EXPECT_EQ(TPC(5.,0.),ms(CVM0+1,CVM0));
    EXPECT_EQ(TPC(5.,0.),ms(CVM0,CVM0+1));
    EXPECT_EQ(TPC(5.,0.),ms(CVM0+1,CVM0+1));
    EXPECT_EQ(TPC(5.,0.),ms(CVM0+1,CVM0+2));

    EXPECT_EQ(TPC(7.,0.),m7(CVM0,CVM0+2));
    EXPECT_EQ(TPC(5.,0.),m7(CVM0+1,CVM0+2));
    EXPECT_EQ(TPC(5.,0.),m7(CVM0+2,CVM0+2));
    EXPECT_EQ(TPC(5.,0.),m7(CVM0+3,CVM0+2));
    EXPECT_EQ(TPC(7.,0.),m7(CVM0+4,CVM0+2));
}

TYPED_TEST(InitializationTest, TestConstVsNonconstforeignArray) {
    TP r[16] = {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.};
    const TP rc[16] = {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.};
    TPC c[16];
    const TPC cc[16];

    basic_srsmatrix<TP> ssr(r, 4);
    basic_srsmatrix<TP> ssrc(rc, 4);
    ssr.set(CVM0+3,CVM0+3,5.11);
    ssrc.set(CVM0+3,CVM0+3,5.11);
    EXPECT_EQ(TP(5.11), r[15]) << "basic_srsmatrix<TP>: non-const foreign array";
    EXPECT_EQ(TP(0.00), rc[15]) << "basic_srsmatrix<TP>: const foreign array";

    basic_schmatrix<TP,TPC> shc(c, 4);
    basic_schmatrix<TP,TPC> shcc(cc, 4);
    shc.set(CVM0+1,CVM0+1,TPC(6.11,0.));
    shcc.set(CVM0+1,CVM0+1,TPC(6.11,0.));
    EXPECT_EQ(TPC(6.11,0.), c[5]) << "basic_schmatrix<TP,TPC>: non-const foreign array";
    EXPECT_EQ(TPC(0.0,0.0), cc[5]) << "basic_schmatrix<TP,TPC>: const foreign array";

    basic_rvector<TP> vr(r, 16);
    basic_rvector<TP> vrc(rc, 16);
    vr[CVM0+7]=3.33;
    vrc[CVM0+7]=3.33;
    EXPECT_EQ(TP(3.33), r[7]) << "basic_rvector<TP>: non-const foreign array";
    EXPECT_EQ(TP(0.00), rc[7]) << "basic_rvector<TP>: const foreign array";

    basic_cvector<TP,TPC> vc(c, 16);
    basic_cvector<TP,TPC> vcc(cc, 16);
    vc[CVM0+7]=TPC(3.33,4.44);
    vcc[CVM0+7]=TPC(3.33,4.44);
    EXPECT_EQ(TPC(3.33,4.44), c[7]) << "basic_cvector<TP,TPC>: non-const foreign array";
    EXPECT_EQ(TPC(0.0,0.0), cc[7]) << "basic_cvector<TP,TPC>: const foreign array";

    basic_rmatrix<TP> mr(r, 3, 4);
    basic_rmatrix<TP> mrc(rc, 3, 4);
    mr(CVM0+1,CVM0+1)=3.22;
    mrc(CVM0+1,CVM0+1)=3.22;
    EXPECT_EQ(TP(3.22), r[4]) << "basic_rmatrix<TP>: non-const foreign array";
    EXPECT_EQ(TP(0.00), rc[4]) << "basic_rmatrix<TP>: const foreign array";

    basic_cmatrix<TP,TPC> mc(c, 3, 4);
    basic_cmatrix<TP,TPC> mcc(cc, 3, 4);
    mc(CVM0+1,CVM0+1)=TPC(3.33,4.22);
    mcc(CVM0+1,CVM0+1)=TPC(3.33,4.22);
    EXPECT_EQ(TPC(3.33,4.22), c[4]) << "basic_cmatrix<TP,TPC>: non-const foreign array";
    EXPECT_EQ(TPC(0.0,0.0), cc[4]) << "basic_cmatrix<TP,TPC>: const foreign array";

    basic_srmatrix<TP> sr(r, 4);
    basic_srmatrix<TP> src(rc, 4);
    sr(CVM0+1,CVM0+1)=3.11;
    src(CVM0+1,CVM0+1)=3.11;
    EXPECT_EQ(TP(3.11), r[5]) << "basic_srmatrix<TP>: non-const foreign array";
    EXPECT_EQ(TP(0.00), rc[5]) << "basic_srmatrix<TP>: const foreign array";

    basic_scmatrix<TP,TPC> sc(c, 4);
    basic_scmatrix<TP,TPC> scc(cc, 4);
    sc(CVM0+1,CVM0+1)=TPC(3.11,4.22);
    scc(CVM0+1,CVM0+1)=TPC(3.11,4.22);
    EXPECT_EQ(TPC(3.11,4.22), c[5]) << "basic_scmatrix<TP,TPC>: non-const foreign array";
    EXPECT_EQ(TPC(0.0,0.0), cc[5]) << "basic_scmatrix<TP,TPC>: const foreign array";

    basic_srbmatrix<TP> br(r, 8, 1, 0);
    basic_srbmatrix<TP> brc(rc, 8, 1, 0);
    br(CVM0+1,CVM0)=3.01;
    brc(CVM0+1,CVM0)=3.01;
    EXPECT_EQ(TP(3.01), r[1]) << "basic_srbmatrix<TP>: non-const foreign array";
    EXPECT_EQ(TP(0.00), rc[1]) << "basic_srbmatrix<TP>: const foreign array";

    basic_scbmatrix<TP,TPC> bc(c, 8, 1, 0);
    basic_scbmatrix<TP,TPC> bcc(cc, 8, 1, 0);
    bc(CVM0+1,CVM0)=TPC(3.11,2.11);
    bcc(CVM0+1,CVM0)=TPC(3.11,2.11);
    EXPECT_EQ(TPC(3.11,2.11), c[1]) << "basic_scbmatrix<TP,TPC>: non-const foreign array";
    EXPECT_EQ(TPC(0.0,0.0), cc[1]) << "basic_scbmatrix<TP,TPC>: const foreign array";
}

TYPED_TEST(InitializationTest, TestMultCrash) {
    basic_cvector<TP,TPC> cv1(this->a1, this->a2, 10);   // note: this constructor copies memory
    basic_cvector<TP,TPC> cv2(this->a1, this->a2, 10, 3);// note: this constructor copies memory
    basic_cmatrix<TP,TPC> cm1(this->a1, this->a2, 2, 3); // note: this constructor copies memory
    basic_cmatrix<TP,TPC> cm2(cm1);
    cv1.set(TPC(2,1));
    cv2.set(TPC(-1,3));
    cm2.set(TPC(-4,3));
    cv1.resize (2);
    cv2.resize (3);
    cv2.mult(cv1, cm2);
    EXPECT_EQ(cv2[CVM0], cv1 * cm2(CVM0));
}

TYPED_TEST(InitializationTest, TestConstructorsAndBasicFeatures) {
    basic_rvector<TP>  rv;
    basic_rvector<TP>  rv0(10);
    basic_rvector<TP>  rv1(this->a1, 10);                 // note: this constructor shares memory
    basic_rvector<TP>  rv2(this->a1, 10, 3);              // note: this constructor shares memory
    basic_rvector<TP>  rv3(11, 17.77);
    basic_rvector<TP>  rv4(rv2);                    // note: this constructor copies memory

    basic_rmatrix<TP> bigm(100, 100);
    basic_cmatrix<TP,TPC> bigcm(100, 100);
    bigm.randomize(0., 2.);
    bigcm.randomize_real(0., 2.);
    bigcm.randomize_imag(0., 2.);

    basic_rmatrix<TP>  rm;
    basic_rmatrix<TP>  rm0(bigm, 21, 34, 5, 6);
    basic_rmatrix<TP>  rm1(this->a1, 2, 3);               // note: this constructor shares memory
    basic_rmatrix<TP>  rm2(rm1);
    basic_rmatrix<TP>  rm3(rv2, true);              // column
    basic_rmatrix<TP>  rm4(rv2, false);             // row
    basic_srmatrix<TP> srm;
    basic_srmatrix<TP> srm0 (bigm, 43, 47, 4);
    basic_srmatrix<TP> srm1 (this->a1, 3);                // note: this constructor shares memory
    basic_srmatrix<TP> srm2 (srm1);
    basic_srmatrix<TP> srm30(srm0);

    basic_cvector<TP,TPC> cv;
    basic_cvector<TP,TPC> cv0 (10);
    basic_cvector<TP,TPC> cv1 (this->a1, this->a2, 10);         // note: this constructor copies memory
    basic_cvector<TP,TPC> cv2 (this->a1, this->a2, 10, 3);      // note: this constructor copies memory
    basic_cvector<TP,TPC> cv3 (this->c1, 10, 3);          // note: this constructor shares memory
    basic_cvector<TP,TPC> cv4 (11, TPC(1.3, 2.4));
    basic_cvector<TP,TPC> cv5 (cv3);
    basic_cvector<TP,TPC> cv6 (rv1, rv2);           // note: this constructor copies memory
    basic_cvector<TP,TPC> cv7 (this->a4, 10, true,  2);   // note: this constructor copies memory
    basic_cvector<TP,TPC> cv8 (this->a4, 10, false, 3);   // note: this constructor copies memory
    basic_cvector<TP,TPC> cv9 (rv2, true);
    basic_cvector<TP,TPC> cv10(rv2, false);

    basic_cmatrix<TP,TPC> cm;
    basic_cmatrix<TP,TPC> cm0(bigcm, 68, 17, 5, 6);
    basic_cmatrix<TP,TPC> cm1(this->a1, this->a2, 2, 3);        // note: this constructor copies memory
    basic_cmatrix<TP,TPC> cm2(cm1);
    basic_scmatrix<TP,TPC> scm;
    basic_scmatrix<TP,TPC> scm0(4);
    basic_scmatrix<TP,TPC> scm1(this->a1, this->a2, 3);         // note: this constructor copies memory
    basic_scmatrix<TP,TPC> scm2(scm1);

    basic_srbmatrix<TP> srbm;
    basic_srbmatrix<TP> srbm1(this->a1, 4, 1, 2);

    basic_scbmatrix<TP,TPC> scbm;
    basic_scbmatrix<TP,TPC> scbm1(this->c1, 4, 1, 2);

    basic_srbmatrix<TP> srbm5(5, 1, 2);
    basic_scbmatrix<TP,TPC> scbm5(5, 1, 2);

    basic_srsmatrix<TP> srs1(3);
    basic_schmatrix<TP,TPC> sch1(3);


// Array<TR,TC> derived features.
    EXPECT_EQ(0,  rv   .size());
    EXPECT_EQ(10, rv0  .size());
    EXPECT_EQ(10, rv1  .size());
    EXPECT_EQ(10, rv2  .size());
    EXPECT_EQ(11, rv3  .size());
    EXPECT_EQ(10, rv4  .size());
    EXPECT_EQ(0,  rm   .size());
    EXPECT_EQ(30, rm0  .size());
    EXPECT_EQ(6,  rm1  .size());
    EXPECT_EQ(6,  rm2  .size());
    EXPECT_EQ(10, rm3  .size());
    EXPECT_EQ(10, rm4  .size());
    EXPECT_EQ(0,  srm  .size());
    EXPECT_EQ(16, srm0 .size());
    EXPECT_EQ(9,  srm1 .size());
    EXPECT_EQ(9,  srm2 .size());
    EXPECT_EQ(0,  cv   .size());
    EXPECT_EQ(10, cv0  .size());
    EXPECT_EQ(10, cv1  .size());
    EXPECT_EQ(10, cv2  .size());
    EXPECT_EQ(10, cv3  .size());
    EXPECT_EQ(11, cv4  .size());
    EXPECT_EQ(10, cv5  .size());
    EXPECT_EQ(10, cv6  .size());
    EXPECT_EQ(10, cv7  .size());
    EXPECT_EQ(10, cv8  .size());
    EXPECT_EQ(10, cv9  .size());
    EXPECT_EQ(10, cv10 .size());
    EXPECT_EQ(0,  cm   .size());
    EXPECT_EQ(30, cm0  .size());
    EXPECT_EQ(6,  cm1  .size());
    EXPECT_EQ(0,  scm  .size());
    EXPECT_EQ(16, scm0 .size());
    EXPECT_EQ(9,  scm1 .size());
    EXPECT_EQ(9,  scm2 .size());
    EXPECT_EQ(0,  srbm .size());
    EXPECT_EQ(16, srbm1.size());
    EXPECT_EQ(0,  scbm .size());
    EXPECT_EQ(16, scbm1.size());
    EXPECT_EQ(20, srbm5.size());
    EXPECT_EQ(20, scbm5.size());
    EXPECT_EQ(9,  srs1.size());
    EXPECT_EQ(9,  sch1.size());

    EXPECT_EQ(0, rv   .incr());
    EXPECT_EQ(1, rv0  .incr());
    EXPECT_EQ(1, rv1  .incr());
    EXPECT_EQ(3, rv2  .incr());
    EXPECT_EQ(1, rv3  .incr());
    EXPECT_EQ(1, rv4  .incr());
    EXPECT_EQ(0, rm   .incr());
    EXPECT_EQ(1, rm0  .incr());
    EXPECT_EQ(1, rm1  .incr());
    EXPECT_EQ(1, rm2  .incr());
    EXPECT_EQ(1, rm3  .incr());
    EXPECT_EQ(1, rm4  .incr());
    EXPECT_EQ(0, srm  .incr());
    EXPECT_EQ(1, srm0 .incr());
    EXPECT_EQ(1, srm1 .incr());
    EXPECT_EQ(1, srm2 .incr());
    EXPECT_EQ(0, cv   .incr());
    EXPECT_EQ(1, cv0  .incr());
    EXPECT_EQ(1, cv1  .incr());
    EXPECT_EQ(1, cv2  .incr());
    EXPECT_EQ(3, cv3  .incr());
    EXPECT_EQ(1, cv4  .incr());
    EXPECT_EQ(1, cv5  .incr());
    EXPECT_EQ(1, cv6  .incr());
    EXPECT_EQ(1, cv7  .incr());
    EXPECT_EQ(1, cv8  .incr());
    EXPECT_EQ(1, cv9  .incr());
    EXPECT_EQ(1, cv10 .incr());
    EXPECT_EQ(0, cm   .incr());
    EXPECT_EQ(1, cm0  .incr());
    EXPECT_EQ(1, cm1  .incr());
    EXPECT_EQ(1, cm2  .incr());
    EXPECT_EQ(0, scm  .incr());
    EXPECT_EQ(1, scm0 .incr());
    EXPECT_EQ(1, scm1 .incr());
    EXPECT_EQ(1, scm2 .incr());
    EXPECT_EQ(0, srbm .incr());
    EXPECT_EQ(1, srbm1.incr());
    EXPECT_EQ(0, scbm .incr());
    EXPECT_EQ(1, scbm1.incr());
    EXPECT_EQ(1, srs1 .incr());
    EXPECT_EQ(1, sch1 .incr());

    basic_cmatrix<TP,TPC> cm1r(rm1);
    basic_cmatrix<TP,TPC> cm1i(rm1, false);
    EXPECT_EQ(TPC(rm1(1,2),0.), cm1r[1][2]) << "basic_cmatrix<TP,TPC>(basic_rmatrix<TP>)";
    EXPECT_EQ(TPC(0.,rm1(1,2)), cm1i[1][2]) << "basic_cmatrix<TP,TPC>(basic_rmatrix<TP>)";

    rm2.resize(4, 4);
    cm2.resize(4, 4);
    basic_srmatrix<TP> srm3 (rm2);
    basic_scmatrix<TP,TPC> scm3 (cm2);

    EXPECT_EQ(16, rm2  .size());
    EXPECT_EQ(16, cm2  .size());
    EXPECT_EQ(1, rm2  .incr());
    EXPECT_EQ(1, cm2  .incr());
    EXPECT_EQ(16, srm3 .size());
    EXPECT_EQ(16, scm3 .size());
    EXPECT_EQ(1, srm3 .incr());
    EXPECT_EQ(1, scm3 .incr());

    basic_scmatrix<TP,TPC> scm4 (srm3, true);
    basic_scmatrix<TP,TPC> scm5 (srm3, false);

    EXPECT_EQ(16, scm4  .size());
    EXPECT_EQ(16, scm5  .size());
    EXPECT_EQ(1 , scm4  .incr());
    EXPECT_EQ(1 , scm5  .incr());

    basic_scmatrix<TP,TPC> scm6 (srm3, srm0);
    EXPECT_EQ(16, scm6  .size());
    EXPECT_EQ(1 , scm6  .incr());

    basic_srmatrix<TP> srm4 (srbm1);
    EXPECT_EQ(16, srm4 .size());
    EXPECT_EQ(1, srm4 .incr());

    basic_scmatrix<TP,TPC> scm8 (scbm1);
    EXPECT_EQ(16, scm8 .size());
    EXPECT_EQ(1, scm8 .incr());

    basic_srmatrix<TP> srm5 (rv2);
    EXPECT_EQ(100, srm5 .size());
    EXPECT_EQ(1, srm5 .incr());

    basic_scmatrix<TP,TPC> scm7 (cv2);
    EXPECT_EQ(100, scm7 .size());
    EXPECT_EQ(1, scm7 .incr());

// Indexing and assignments
    EXPECT_EQ(1., rv2[CVM0]);
    EXPECT_EQ(28., rv2[CVM0+9]);

    TP r1 = -1.92;
    this->a1[3] = r1;
    EXPECT_EQ(r1, rv1[CVM0+3]);
    EXPECT_EQ(r1, rv2[CVM0+1]);
    EXPECT_EQ(r1, rm1[CVM0+1][CVM0+1]);
    EXPECT_EQ(r1, rm1(CVM0+1,CVM0+1));
    EXPECT_EQ(r1, srm1[CVM0][CVM0+1]);
    EXPECT_EQ(r1, srm1(CVM0, CVM0+1));

    EXPECT_EQ(TPC(4.,0.4), cm1(CVM0+1, CVM0+1));

    TP* pr1 = srm30;
    srm30(CVM0+3,CVM0+3) = r1;
    EXPECT_EQ(r1, srm30(CVM0+3,CVM0+3));
    EXPECT_EQ(r1, pr1[15]);

    TP* pr2 = srbm1;
    srbm1(CVM0,CVM0+1) = r1;
    EXPECT_EQ(r1, srbm1(CVM0,CVM0+1));
    EXPECT_EQ(r1, pr2[5]);

    TPC cr1 = TPC(1.07, -0.179);
    TPC* pc2 = scbm1;
    scbm1(CVM0,CVM0+1) = cr1;
    EXPECT_EQ(cr1, scbm1(CVM0,CVM0+1));
    EXPECT_EQ(cr1, pc2[5]);

    srs1.assign(this->as);
    sch1.assign((TPC*)this->cs);

    EXPECT_EQ(this->as[3], srs1[CVM0][CVM0+1]);
    EXPECT_EQ(this->as[5], srs1(CVM0+2,CVM0+1));
    EXPECT_EQ(this->as[7], srs1(CVM0+2)[CVM0+1]);

    EXPECT_EQ(TPC(this->cs[6],this->cs[7]), sch1[CVM0][CVM0+1]);
    EXPECT_EQ(TPC(this->cs[10],this->cs[11]), sch1(CVM0+2,CVM0+1));
    EXPECT_EQ(TPC(this->cs[14],this->cs[15]), sch1(CVM0+2)[CVM0+1]);


// Array<TR,TC> derived features -  continued
    rv << rv1.normalize();
    EXPECT_EQ(rv1[CVM0+6], rv(CVM0+6));

    TP r2 = 0.;
    for (int i = 0; i < 10; ++i) {
        r2 += this->a1[i] * this->a1[i];
    }

    EXPECT_NEAR(1., r2, sp<TP>());
    EXPECT_NEAR(1., rv.norm(), s<TP>());

    EXPECT_EQ(CVM0+9, rv.indofmax ());
    EXPECT_EQ(CVM0 , rv.indofmin ());

    r1 = rv[CVM0+9];
    EXPECT_EQ(r1, rv.norminf ());

    rv1.sum (rv1, rv);
    EXPECT_EQ(r1 + r1, rv1[CVM0+9]);
    rv1.diff (rv1, rv);
    EXPECT_EQ(r1, rv1[CVM0+9]);

    rv1 += rv;
    EXPECT_EQ(r1 + r1, rv1[CVM0+9]);
    rv1 -= rv;
    EXPECT_EQ(r1, rv1[CVM0+9]);
    rv1 += rv1;
    EXPECT_EQ(r1 + r1, rv1[CVM0+9]);

    cv << cv1;
    EXPECT_EQ(cv1[CVM0+6], cv(CVM0+6));

    cr1 = cv1[CVM0+9];
    cv1 += cv;
    EXPECT_EQ(cr1 + cr1, cv1[CVM0+9]);
    cv1 -= cv;
    EXPECT_EQ(cr1, cv1[CVM0+9]);
    cv1 += cv1;
    EXPECT_EQ(cr1 + cr1, cv1[CVM0+9]);

    rm << rm1;
    EXPECT_EQ(rm1[CVM0+1][CVM0], rm(CVM0+1,CVM0));

    r1 = rm(CVM0+1,CVM0+1);
    rm += rm1;
    EXPECT_EQ(r1 + r1, rm(CVM0+1,CVM0+1));
    rm -= rm1;
    EXPECT_EQ(r1, rm(CVM0+1,CVM0+1));
    rm += rm;
    EXPECT_EQ(r1 + r1, rm(CVM0+1,CVM0+1));

    cm << cm1;
    EXPECT_EQ(cm1[CVM0+1][CVM0], cm(CVM0+1,CVM0));

    cr1 = cm(CVM0+1,CVM0+1);
    cm += cm1;
    EXPECT_EQ(cr1 + cr1, cm(CVM0+1,CVM0+1));
    cm -= cm1;
    EXPECT_EQ(cr1, cm(CVM0+1,CVM0+1));
    cm += cm;
    EXPECT_EQ(cr1 + cr1, cm(CVM0+1,CVM0+1));

    srm << srm1;
    r1 = srm(CVM0+1,CVM0+1);
    srm += srm1;
    EXPECT_EQ(r1 + r1, srm(CVM0+1,CVM0+1));
    srm -= srm1;
    EXPECT_EQ(r1, srm(CVM0+1,CVM0+1));
    srm += srm;
    EXPECT_EQ(r1 + r1, srm(CVM0+1,CVM0+1));

    scm << scm1;
    EXPECT_EQ(scm1[CVM0+1][CVM0], scm(CVM0+1,CVM0));

    cr1 = scm(CVM0+1,CVM0+1);
    scm += scm1;
    EXPECT_EQ(cr1 + cr1, scm(CVM0+1,CVM0+1));
    scm -= scm1;
    EXPECT_EQ(cr1, scm(CVM0+1,CVM0+1));
    scm += scm;
    EXPECT_EQ(cr1 + cr1, scm(CVM0+1,CVM0+1));


    srbm1.set(1.14);
    srbm << srbm1;
    srbm.set( -.684);
    r1 = srbm(CVM0+1,CVM0);
    r2 = srbm1(CVM0+1,CVM0);
    srbm += srbm1;
    EXPECT_EQ(r1 + r2, srbm(CVM0+1,CVM0));
    srbm -= srbm1;
    EXPECT_EQ(r1, srbm(CVM0+1,CVM0));

    TPC cr2 = TPC(1.03, -0.79);
    scbm1.set(cr2);
    scbm << scbm1;
    scbm.set(cr1);
    cr1 = scbm(CVM0+1,CVM0);
    cr2 = scbm1(CVM0+1,CVM0);
    scbm += scbm1;

    EXPECT_EQ(cr1 + cr2, scbm(CVM0+1,CVM0));
    scbm -= scbm1;
    EXPECT_EQ(cr1, scbm(CVM0+1,CVM0));


    basic_srsmatrix<TP> srs2;
    basic_schmatrix<TP,TPC> sch2;
    srs2 << srs1;
    sch2 << sch1;
    EXPECT_EQ(0. , (srs2 - srs1).norm());
    EXPECT_EQ(0. , (sch2 - sch1).norm());

    basic_srsmatrix<TP> srs2sub (srs2, CVM0+1, 2);
    EXPECT_EQ(srs2(CVM0+1,CVM0+2), srs2sub(CVM0,CVM0+1));
    basic_schmatrix<TP,TPC> sch2sub (sch2, CVM0+1, 2);
    EXPECT_EQ(sch2(CVM0+1,CVM0+2), sch2sub(CVM0,CVM0+1));


    r2 = 1.13;
    cr2 = TPC(1.03, -0.79);

    TP rs1 = srs2(CVM0+1,CVM0+2);
    TPC cs1 = sch2(CVM0+1,CVM0+2);
    srs2 *= r2;
    sch2 *= r2;
    EXPECT_EQ(rs1 * r2 , srs2(CVM0+1,CVM0+2));
    EXPECT_EQ(cs1 * r2 , sch2(CVM0+1,CVM0+2));
    srs2 /= r2;
    sch2 /= r2;
    EXPECT_EQ(rs1, srs2(CVM0+1,CVM0+2));
    EXPECT_EQ(cs1, sch2(CVM0+1,CVM0+2));
    EXPECT_EQ(cs1 * cr2, (sch2 * cr2)(CVM0+1,CVM0+2));

    basic_rvector<TP> vrs1(3);
    vrs1.randomize(3., 7.);
    EXPECT_EQ(0., (srs1 * vrs1 - basic_srmatrix<TP>(srs1) * vrs1).norm());

    basic_cvector<TP,TPC> vch1(3);
    vch1.randomize_real(3., 7.);
    vch1.randomize_imag(-3., 7.);
    EXPECT_EQ(0., (sch1 * vch1 - basic_scmatrix<TP,TPC>(sch1) * vch1).norm());

    r1 = rv1(CVM0+8);
    rv1 *= r2;
    EXPECT_EQ(r1 * r2, rv1(CVM0+8));
    r1 = rm3(CVM0+6,CVM0);
    rm3 *= r2;
    EXPECT_EQ(r1 * r2, rm3(CVM0+6,CVM0));
    r1 = srm4(CVM0,CVM0+1);
    srm4 *= r2;
    EXPECT_EQ(r1 * r2, srm4(CVM0,CVM0+1));
    r1 = srbm1(CVM0,CVM0+1);
    srbm1 *= r2;
    EXPECT_EQ(r1 * r2, srbm1(CVM0,CVM0+1));
    cr1 = scbm1(CVM0,CVM0+1);
    scbm1 *= cr2;
    EXPECT_EQ(cr1 * cr2, scbm1(CVM0,CVM0+1));
    r1 = rv1(CVM0+8);
    rv1 /= r2;
    EXPECT_EQ(r1 / r2, rv1(CVM0+8));
    r1 = rm3(CVM0+6,CVM0);

    rm3 /= r2;
    EXPECT_EQ(r1 / r2, rm3(CVM0+6,CVM0));
    r1 = srm4(CVM0,CVM0+1);
    srm4 /= r2;
    EXPECT_EQ(r1 / r2, srm4(CVM0,CVM0+1));
    r1 = srbm1(CVM0,CVM0+1);
    srbm1 /= r2;
    EXPECT_EQ(r1 / r2, srbm1(CVM0,CVM0+1));
    cr1 = scbm1(CVM0+1,CVM0);
    scbm1 /= cr2;
    EXPECT_NEAR(std::abs(cr1 / cr2), std::abs(scbm1(CVM0+1,CVM0)), sp<TP>());


    cr1 = cv1(CVM0+8);
    cv1 *= r2;
    EXPECT_EQ(cr1 * r2, cv1(CVM0+8));
    cr1 = cm1(CVM0+1,CVM0+1);
    cm1 *= r2;
    EXPECT_EQ(cr1 * r2, cm1(CVM0+1,CVM0+1));
    cr1 = scm1(CVM0+1,CVM0+1);
    scm1 *= r2;
    EXPECT_EQ(cr1 * r2, scm1(CVM0+1,CVM0+1));
    cr1 = scbm1(CVM0,CVM0+1);
    scbm1 *= r2;
    EXPECT_EQ(cr1 * r2, scbm1(CVM0,CVM0+1));
    cr1 = cv1(CVM0+8);
    cv1 /= r2;
    EXPECT_NEAR(std::abs(cr1 / r2), std::abs(cv1(CVM0+8)), s<TP>());
    cr1 = cm1(CVM0+1,CVM0+1);
    cm1 /= r2;
    EXPECT_NEAR(std::abs(cr1 / r2), std::abs(cm1(CVM0+1,CVM0+1)), s<TP>());
    cr1 = scm1(CVM0+1,CVM0+1);
    scm1 /= r2;
    EXPECT_EQ(cr1 / r2, scm1(CVM0+1,CVM0+1));
    cr1 = scbm1(CVM0+1,CVM0);
    scbm1 /= r2;
    EXPECT_NEAR(std::abs(cr1 / r2), std::abs(scbm1(CVM0+1,CVM0)), s<TP>());


    cr2 = TPC(1.03, -0.79);

    cr1 = cv1(CVM0+8);
    cv1 *= cr2;
    EXPECT_EQ(cr1 * cr2, cv1(CVM0+8));
    cr1 = cm1(CVM0+1,CVM0+1);
    cm1 *= cr2;
    EXPECT_EQ(cr1 * cr2, cm1(CVM0+1,CVM0+1));
    cr1 = scm1(CVM0+1,CVM0+1);
    scm1 *= cr2;
    EXPECT_EQ(cr1 * cr2, scm1(CVM0+1,CVM0+1));
    cr1 = scbm1(CVM0+1,CVM0);
    scbm1 *= cr2;
    EXPECT_EQ(cr1 * cr2, scbm1(CVM0+1,CVM0));
    cr1 = cv1(CVM0+8);
    cv1 /= cr2;
    EXPECT_NEAR(std::abs(cr1 / cr2), std::abs(cv1(CVM0+8)), s<TP>());
    cr1 = cm1(CVM0+1,CVM0+1);
    cm1 /= cr2;
    EXPECT_NEAR(std::abs(cr1 / cr2), std::abs(cm1(CVM0+1,CVM0+1)), s<TP>());
    cr1 = scm1(CVM0+1,CVM0+1);
    scm1 /= cr2;
    EXPECT_NEAR(std::abs(cr1 / cr2), std::abs(scm1(CVM0+1,CVM0+1)), s<TP>());
    cr1 = scbm1(CVM0,CVM0+1);
    scbm1 /= cr2;
    EXPECT_NEAR(std::abs(cr1 / cr2), std::abs(scbm1(CVM0+1,CVM0+1)), sp<TP>());

    srbm << srbm1;
    EXPECT_EQ(srbm1(CVM0+1,CVM0+2), srbm(CVM0+1,CVM0+2));
    EXPECT_EQ(srbm1(CVM0,CVM0+3), srbm(CVM0,CVM0+3));
    scbm << scbm1;
    EXPECT_EQ(scbm1(CVM0+1,CVM0+2), scbm(CVM0+1,CVM0+2));
    EXPECT_EQ(scbm1(CVM0,CVM0+3), scbm(CVM0,CVM0+3));

    srs2.set(2.3);
    EXPECT_EQ(TP(2.3), srs2(CVM0,CVM0+2));
    EXPECT_EQ(TP(2.3), srs2(CVM0+2,CVM0+1));

    sch2.set_real(2.3);
    EXPECT_EQ(TP(2.3), sch2.real()(CVM0,CVM0+2));
    EXPECT_EQ(TP(2.3), sch2.real()(CVM0+2,CVM0+1));

    r1 = -0.127;
    rv.set(r1);
    EXPECT_EQ(r1, rv[CVM0]);
    rm.set(r1);
    EXPECT_EQ(r1, rm[CVM0][CVM0+1]);
    srm.set(r1);
    EXPECT_EQ(r1, srm(CVM0+1,CVM0+1));
    srbm.set(r1);
    EXPECT_EQ(r1, srbm(CVM0+1,CVM0+2));
    EXPECT_EQ(0, srbm(CVM0,CVM0+3));

    cr2 = TPC(1.3, -0.9);
    cv.set(cr1);
    EXPECT_EQ(cr1, cv[CVM0+6]);
    cm.set(cr1);
    EXPECT_EQ(cr1, cm[CVM0+1][CVM0+2]);
    scm.set(cr1);
    EXPECT_EQ(cr1, scm(CVM0+2,CVM0+1));
    scbm.set(cr1);
    EXPECT_EQ(cr1, scbm(CVM0+2,CVM0+1));

    rv.assign(CVM0+1, this->a2);
    EXPECT_EQ(this->a2[1], rv[CVM0+2]);
    rv.assign(this->a2);
    EXPECT_EQ(this->a2[2], rv[CVM0+2]);
    rm.assign(this->a2);
    EXPECT_EQ(this->a2[4], rm(CVM0,CVM0+2));
    srm.assign(this->a2);
    EXPECT_EQ(this->a2[8], srm(CVM0+2,CVM0+2));
    srbm.assign(this->a2);
    EXPECT_EQ(this->a2[2], srbm(CVM0,CVM0));
    EXPECT_EQ(this->a2[9], srbm(CVM0+1,CVM0+2));

    cv.assign(CVM0+1, this->c1);
    EXPECT_EQ(this->c1[1], cv[CVM0+2]);
    cv.assign(this->c1);
    EXPECT_EQ(this->c1[2], cv[CVM0+2]);
    cm.assign(this->c1);
    EXPECT_EQ(this->c1[4], cm(CVM0,CVM0+2));
    scm.assign(this->c1);
    EXPECT_EQ(this->c1[8], scm(CVM0+2,CVM0+2));
    scbm.assign(this->c1);
    EXPECT_EQ(this->c1[2], scbm(CVM0,CVM0));
    EXPECT_EQ(this->c1[9], scbm(CVM0+1,CVM0+2));












        srs2 = srs1;
        EXPECT_TRUE(srs1 == srs2) << "srsmatrix ==";
        srs2.set(CVM0+1, CVM0+2, srs2(CVM0+1,CVM0+2) + 0.000001);
        EXPECT_FALSE(srs1 == srs2) << "srsmatrix ==";
        EXPECT_TRUE(srs1 != srs2) << "srsmatrix !=";

        sch2 = sch1;
        EXPECT_TRUE(sch1 == sch2) << "schmatrix ==";
        sch2.set(CVM0+1, CVM0+2, sch2(CVM0+1,CVM0+2) + TPC(0.000001,0.00001));
        EXPECT_FALSE(sch1 == sch2) << "schmatrix ==";
        EXPECT_TRUE(sch1 != sch2) << "schmatrix !=";



        rv1 = rv;
        EXPECT_EQ(rv(CVM0+2), rv1[CVM0+2]) << "rvector = rvector";
        EXPECT_TRUE(rv1 == rv) << "rvector ==";
        EXPECT_FALSE(rv1 != rv) << "rvector !=";
        rm1 = rm;
        EXPECT_EQ(rm(CVM0+1,CVM0+2), rm1[CVM0+1][CVM0+2]) << "rmatrix = rmatrix";
        EXPECT_TRUE(rm1 == rm) << "rmatrix ==";
        EXPECT_FALSE(rm1 != rm) << "rmatrix !=";
        EXPECT_TRUE(rm1[CVM0] == rm[CVM0]) << "rmatrix = rmatrix, rm1[1] == rm[1]";
        EXPECT_FALSE(rm1(CVM0+1) != rm(CVM0+1)) << "rmatrix = rmatrix,rm1(2) != rm(2)";
        srm1 = srm;
        EXPECT_EQ(srm(CVM0+1,CVM0+2), srm1[CVM0+1][CVM0+2]) << "srmatrix = srmatrix";
        EXPECT_TRUE(srm1 == srm) << "srmatrix ==";
        EXPECT_FALSE(srm1 != srm) << "srmatrix !=";
        EXPECT_TRUE(srm1[CVM0] == srm[CVM0]) << "srmatrix = srmatrix, srm1[1] == srm[1]";
        EXPECT_FALSE(srm1(CVM0+1) != srm(CVM0+1)) << "srmatrix = srmatrix,srm1(2) != srm(2)";
        srbm1 = srbm;
        EXPECT_EQ(srbm(CVM0+1,CVM0), srbm1[CVM0+1][CVM0]) << "srbmatrix = srbmatrix";
        EXPECT_TRUE(srbm1 == srbm) << "srbmatrix ==";
        EXPECT_FALSE(srbm1 != srbm) << "srbmatrix !=";
        EXPECT_TRUE(srbm1[CVM0] == srbm[CVM0]) << "srbmatrix = srbmatrix, srbm1[1] == srbm[1]";
        EXPECT_FALSE(srbm1(CVM0+1) != srbm(CVM0+1)) << "srbmatrix = srbmatrix,srbm1(2) != srbm(2)";

        cv1 = cv;
        EXPECT_EQ(cv(CVM0+3), cv1[CVM0+3]) << "cvector = cvector";
        EXPECT_TRUE(cv1 == cv) << "cvector ==";
        EXPECT_FALSE(cv1 != cv) << "cvector !=";
        cm1 = cm;
        EXPECT_EQ(cm(CVM0+1,CVM0), cm1[CVM0+1][CVM0]) << "cmatrix = cmatrix";
        EXPECT_TRUE(cm1 == cm) << "cmatrix ==";
        EXPECT_FALSE(cm1 != cm) << "cmatrix !=";
        EXPECT_TRUE(cm1[CVM0] == cm[CVM0]) << "cmatrix = cmatrix, cm1[1] == cm[1]";
        EXPECT_FALSE(cm1(CVM0+1) != cm(CVM0+1)) << "cmatrix = cmatrix,cm1(2) != cm(2)";
        scm1 = scm;
        EXPECT_EQ(scm(CVM0+1,CVM0), scm1[CVM0+1][CVM0]) << "scmatrix = scmatrix";
        EXPECT_TRUE(scm1 == scm) << "scmatrix ==";
        EXPECT_FALSE(scm1 != scm) << "scmatrix !=";
        EXPECT_TRUE(scm1[CVM0] == scm[CVM0]) << "scmatrix = scmatrix, scm1[1] == scm[1]";
        EXPECT_FALSE(scm1(CVM0+1) != scm(CVM0+1)) << "scmatrix = scmatrix,scm1(2) != scm(2)";
        scbm1 = scbm;
        EXPECT_EQ(scbm(CVM0+1,CVM0), scbm1[CVM0+1][CVM0]) << "scbmatrix = scbmatrix";
        EXPECT_TRUE(scbm1 == scbm) << "scbmatrix ==";
        EXPECT_FALSE(scbm1 != scbm) << "scbmatrix !=";
        EXPECT_TRUE(scbm1[CVM0] == scbm[CVM0]) << "scbmatrix = scbmatrix, scbm1[1] == scbm[1]";
        EXPECT_FALSE(scbm1(CVM0+1) != scbm(CVM0+1)) << "scbmatrix = scbmatrix,scbm1(2) != scbm(2)";

//        rv2 = rv + rv1;   // wouldn't work because rv and rv1 share the same array!
        rv3.resize(10);
        rv3 = rv + rv1;
        EXPECT_EQ(rv(CVM0) + rv1[CVM0], rv3[CVM0]) << "rvector + rvector";
        EXPECT_EQ(rv(CVM0+9) + rv1[CVM0+9], rv3[CVM0+9]) << "rvector + rvector";
        rv3 = rv - rv1;
        EXPECT_EQ(rv(CVM0) - rv1[CVM0], rv3[CVM0]) << "rvector - rvector";
        EXPECT_EQ(rv(CVM0+9) - rv1[CVM0+9], rv3[CVM0+9]) << "rvector - rvector";
        cv3 = cv + cv1;
        EXPECT_EQ(cv(CVM0) + cv1[CVM0], cv3[CVM0]) << "cvector + cvector";
        EXPECT_EQ(cv(CVM0+9) + cv1[CVM0+9], cv3[CVM0+9]) << "cvector + cvector";
        cv3 = cv - cv1;
        EXPECT_EQ(cv(CVM0) - cv1[CVM0], cv3[CVM0]) << "cvector - cvector";
        EXPECT_EQ(cv(CVM0+9) - cv1[CVM0+9], cv3[CVM0+9]) << "cvector - cvector";
        rm2.resize(2, 3);
        rm = rm1 + rm2;
        EXPECT_EQ(rm1(CVM0,CVM0) + rm2(CVM0, CVM0), rm[CVM0][CVM0]) << "rmatrix + rmatrix";
        EXPECT_EQ((rm1[CVM0+1] + rm2[CVM0+1]).norm(),rm[CVM0+1].norm()) << "rmatrix + rmatrix";
        rm = rm1 - rm2;
        EXPECT_EQ(rm1(CVM0,CVM0) - rm2(CVM0, CVM0), rm[CVM0][CVM0]) << "rmatrix - rmatrix";
        EXPECT_EQ((rm1(CVM0+2) - rm2(CVM0+2)).norm(),rm(CVM0+2).norm()) << "rmatrix - rmatrix";
        cm2.resize(2, 3);
        cm = cm1 + cm2;
        EXPECT_EQ(cm1(CVM0,CVM0) + cm2(CVM0, CVM0), cm[CVM0][CVM0]) << "cmatrix + cmatrix";
        EXPECT_EQ((cm1[CVM0+1] + cm2[CVM0+1]).norm(),cm[CVM0+1].norm()) << "cmatrix + cmatrix";
        cm = cm1 - cm2;
        EXPECT_EQ(cm1(CVM0,CVM0) - cm2(CVM0, CVM0), cm[CVM0][CVM0]) << "cmatrix - cmatrix";
        EXPECT_EQ((cm1(CVM0+2) - cm2(CVM0+2)).norm(),cm(CVM0+2).norm()) << "cmatrix - cmatrix";
        srm = srm1 + srm2;
        EXPECT_EQ(srm1(CVM0,CVM0) + srm2(CVM0, CVM0), srm[CVM0][CVM0]) << "srmatrix + srmatrix";
        EXPECT_EQ((srm1[CVM0+2] + srm2[CVM0+2]).norm(),srm[CVM0+2].norm()) << "srmatrix + srmatrix";
        srm = srm1 - srm2;
        EXPECT_EQ(srm1(CVM0,CVM0) - srm2(CVM0, CVM0), srm[CVM0][CVM0]) << "srmatrix - srmatrix";
        EXPECT_EQ((srm1(CVM0+2) - srm2(CVM0+2)).norm(),srm(CVM0+2).norm()) << "srmatrix - srmatrix";
        scm = scm1 + scm2;
        EXPECT_EQ(scm1(CVM0,CVM0) + scm2(CVM0, CVM0), scm[CVM0][CVM0]) << "scmatrix + scmatrix";
        EXPECT_EQ((scm1(CVM0+2) + scm2(CVM0+2)).norm(),scm(CVM0+2).norm()) << "scmatrix + scmatrix";
        scm = scm1 - scm2;
        EXPECT_EQ(scm1(CVM0,CVM0) - scm2(CVM0, CVM0), scm[CVM0][CVM0]) << "scmatrix - scmatrix";
        EXPECT_EQ((scm1(CVM0+2) - scm2(CVM0+2)).norm(),scm(CVM0+2).norm()) << "scmatrix - scmatrix";

        basic_srbmatrix<TP> srbm2 (this->a2, 4, 1, 2);
        srbm = srbm1 + srbm2;
        EXPECT_EQ(srbm1(CVM0,CVM0) + srbm2(CVM0, CVM0), srbm[CVM0][CVM0]) << "srbmatrix + srbmatrix";
        EXPECT_EQ((srbm1(CVM0+2) + srbm2(CVM0+2)).norm(),srbm(CVM0+2).norm()) << "srbmatrix + srbmatrix";
        srbm = srbm1 - srbm2;
        EXPECT_EQ(srbm1(CVM0,CVM0) - srbm2(CVM0, CVM0), srbm[CVM0][CVM0]) << "srbmatrix - srbmatrix";
        EXPECT_EQ((srbm1(CVM0+2) - srbm2(CVM0+2)).norm(),srbm(CVM0+2).norm()) << "srbmatrix - srbmatrix";

        scbmatrix scbm2 (this->c1, 4, 1, 2);
        scbm = scbm1 + scbm2;
        EXPECT_EQ(scbm1(CVM0,CVM0) + scbm2(CVM0, CVM0), scbm[CVM0][CVM0]) << "scbmatrix + scbmatrix";
        EXPECT_EQ((scbm1(CVM0+2) + scbm2(CVM0+2)).norm(),scbm(CVM0+2).norm()) << "scbmatrix + scbmatrix";
        scbm = scbm1 - scbm2;
        EXPECT_EQ(scbm1(CVM0,CVM0) - scbm2(CVM0, CVM0), scbm[CVM0][CVM0]) << "scbmatrix - scbmatrix";
        EXPECT_EQ((scbm1(CVM0+2) - scbm2(CVM0+2)).norm(),scbm(CVM0+2).norm()) << "scbmatrix - scbmatrix";

        srs2 = srs1;
        rs1 = srs1(CVM0, CVM0+1);
        EXPECT_EQ(rs1 + rs1,(srs1 + srs2)(CVM0,CVM0+1)) << "srsmatrix + srsmatrix";
        EXPECT_EQ(0.,(srs1 - srs2).norm()) << "srsmatrix - srsmatrix";

        sch2 = sch1;
        cs1 = sch1(CVM0, CVM0+1);
        EXPECT_EQ(cs1 + cs1,(sch1 + sch2)(CVM0,CVM0+1)) << "schmatrix + schmatrix";
        EXPECT_EQ(0.,(sch1 - sch2).norm()) << "schmatrix - schmatrix";


        int n1 = -2;
        r1     = -2.;
        rv1 = rv * r1;
        rv3 = n1 * rv;
        EXPECT_EQ(rv1[CVM0+2], rv3[CVM0+2]) << "rvector * number";
        rv3 = r1 * rv;
        EXPECT_EQ(rv1[CVM0+2], rv3[CVM0+2]) << "rvector * number";
        cv1 = cv * r1;
        cv3 = n1 * cv;
        EXPECT_EQ(cv1[CVM0+2], cv3[CVM0+2]) << "cvector * number";
        cv3 = r1 * cv;
        EXPECT_EQ(cv1[CVM0+2], cv3[CVM0+2]) << "cvector * number";
        rm1 = rm * r1;
        rm2 = n1 * rm;
        EXPECT_EQ(rm2(CVM0+1,CVM0+2), rm1(CVM0+1,CVM0+2)) << "rmatrix * number";
        rm2 = r1 * rm;
        EXPECT_EQ(rm2(CVM0+1,CVM0+2), rm1(CVM0+1,CVM0+2)) << "rmatrix * number";
        cm1 = cm * r1;
        cm2 = n1 * cm;
        EXPECT_EQ(cm1(CVM0+1,CVM0+2), cm2(CVM0+1,CVM0+2)) << "cmatrix * number";
        cm2 = r1 * cm;
        EXPECT_EQ(cm1(CVM0+1,CVM0+2), cm2(CVM0+1,CVM0+2)) << "cmatrix * number";
        srm1 = srm * r1;
        srm2 = n1 * srm;
        EXPECT_EQ(srm2(CVM0+1,CVM0+2), srm1(CVM0+1,CVM0+2)) << "srmatrix * number";
        srm2 = r1 * srm;
        EXPECT_EQ(srm2(CVM0+1,CVM0+2), srm1(CVM0+1,CVM0+2)) << "srmatrix * number";
        scm.assign(this->c1);
        scm1 = scm * r1;
        scm2 = n1 * scm;
        EXPECT_EQ(scm1(CVM0+1,CVM0+2), scm2(CVM0+1,CVM0+2)) << "scmatrix * number";
        scm2 = r1 * scm;
        EXPECT_EQ(scm1(CVM0+1,CVM0+2), scm2(CVM0+1,CVM0+2)) << "scmatrix * number";
        srbm1 = srbm * r1;
        srbm2 = n1 * srbm;
        EXPECT_EQ(srbm2(CVM0+1,CVM0+2), srbm1(CVM0+1,CVM0+2)) << "srbmatrix * number";
        srbm2 = r1 * srbm;
        EXPECT_EQ(srbm2(CVM0+1,CVM0+2), srbm1(CVM0+1,CVM0+2)) << "srbmatrix * number";
        scbm1 = scbm * r1;
        scbm2 = n1 * scbm;
        EXPECT_EQ(scbm2(CVM0+1,CVM0+2), scbm1(CVM0+1,CVM0+2)) << "scbmatrix * number";
        scbm2 = r1 * scbm;
        EXPECT_EQ(scbm2(CVM0+1,CVM0+2), scbm1(CVM0+1,CVM0+2)) << "scbmatrix * number";
        cr1 = r1;
        scbm2 = cr1 * scbm;
        EXPECT_EQ(scbm2(CVM0+1,CVM0+2), scbm1(CVM0+1,CVM0+2)) << "scbmatrix * number";

        rv1 = rv / r1;
        EXPECT_EQ(rv[CVM0+9] / r1, rv1[CVM0+9]) << "rvector / number";
        cv1 = cv / r1;
        EXPECT_EQ(cv[CVM0+9] / r1, cv1[CVM0+9]) << "cvector / number";
        rm1 = rm / r1;
        EXPECT_EQ(rm(CVM0+1,CVM0+2) / r1, rm1(CVM0+1,CVM0+2)) << "rmatrix / number";
        cm1 = cm / r1;
        EXPECT_EQ(cm(CVM0+1,CVM0+2) / r1, cm1(CVM0+1,CVM0+2)) << "cmatrix / number";

        srm1 = srm / r1;
        EXPECT_EQ(srm(CVM0+1,CVM0+2) / r1, srm1(CVM0+1,CVM0+2)) << "srmatrix / number";
        scm1 = scm / r1;
        EXPECT_EQ(scm(CVM0+1,CVM0+2) / r1, scm1(CVM0+1,CVM0+2)) << "scmatrix / number";
        srbm1 = srbm / r1;
        EXPECT_EQ(srbm(CVM0+1,CVM0+2) / r1, srbm1(CVM0+1,CVM0+2)) << "srbmatrix / number";
        scbm1 = scbm / r1;
        EXPECT_EQ(scbm(CVM0+1,CVM0+2) / r1, scbm1(CVM0+1,CVM0+2)) << "scbmatrix / number";
        scbm1 = scbm / cr1;
        EXPECT_EQ(scbm(CVM0+1,CVM0+2) / cr1, scbm1(CVM0+1,CVM0+2)) << "scbmatrix / number";

        cv1 = cv  * cr2;
        cv3 = cr2 * cv;
        EXPECT_EQ(cv1[CVM0+2], cv3[CVM0+2]) << "cvector * cmplx number";
        cm1 = cm * cr2;
        cm2 = cr2 * cm;
        EXPECT_EQ(cm1(CVM0+1,CVM0+2), cm2(CVM0+1,CVM0+2)) << "cmatrix * cmplx number";
        scm1 = scm * cr2;
        scm2 = cr2 * scm;
        EXPECT_EQ(scm1(CVM0+1,CVM0+2), scm2(CVM0+1,CVM0+2)) << "scmatrix * cmplx number";
        scbm1 = scbm * cr2;
        scbm2 = cr2 * scbm;
        EXPECT_EQ(scbm1(CVM0+1,CVM0+2), scbm2(CVM0+1,CVM0+2)) << "scbmatrix * cmplx number";

        rv1 = - rv;
        EXPECT_EQ(- rv[CVM0+9], rv1[CVM0+9]) << "- rvector";
        cv1 = - cv;
        EXPECT_EQ(- cv[CVM0+9], cv1[CVM0+9]) << "- cvector";
        rm1 = - rm;
        EXPECT_EQ(- rm(CVM0+1,CVM0+2), rm1(CVM0+1,CVM0+2)) << "- rmatrix";
        cm1 = - cm;
        EXPECT_EQ(- cm(CVM0+1,CVM0+2), cm1(CVM0+1,CVM0+2)) << "- cmatrix";
        srm1 = - srm;
        EXPECT_EQ(- srm(CVM0+1,CVM0+2), srm1(CVM0+1,CVM0+2)) << "- srmatrix";
        scm1 = - scm;
        EXPECT_EQ(- scm(CVM0+1,CVM0+2), scm1(CVM0+1,CVM0+2)) << "- scmatrix";
        srbm.assign(this->a2);
        srbm1 = - srbm;
        EXPECT_EQ(- srbm(CVM0+1,CVM0+2), srbm1(CVM0+1,CVM0+2)) << "- srbmatrix";
        scbm.assign(this->c1);
        scbm1 = - scbm;
        EXPECT_EQ(- scbm(CVM0+1,CVM0+2), scbm1(CVM0+1,CVM0+2)) << "- scbmatrix";


        rv1.set(1.17);
        rv2.set(-0.31);
        rm2.set(9.01);
        srbm1.set(13.1);
        srbm2.set(5.51);
        cv1.set(TPC(2,1));
        cv2.set(TPC(-1,3));
        cm2.set(TPC(-4,3));
        rv1.resize (2);
        rv2.resize (3);
        rv2.mult (rv1, rm2);

        EXPECT_EQ(rv1 * rm2(CVM0), rv2[CVM0]) << "mult";
        rv1.mult (rm2, rv2);
        EXPECT_EQ(rv2 * rm2[CVM0], rv1[CVM0]) << "mult";

        cv1.resize (2);
        cv2.resize (3);
        cv2.mult (cv1, cm2);

        EXPECT_EQ(cv1 * cm2(CVM0), cv2[CVM0]) << "mult";
        cv1.mult (cm2, cv2);
        EXPECT_EQ(cv2 * cm2[CVM0], cv1[CVM0]) << "mult";

        rv1.resize (3);
        rv1.mult (srm2, rv2);
        EXPECT_EQ(rv2 * srm2[CVM0], rv1[CVM0]) << "mult";
        rv2.mult (rv1, srm2);

        EXPECT_EQ(rv1 * srm2(CVM0), rv2[CVM0]) << "mult";

        cv1.resize (3);
        cv1.mult (scm2, cv2);
        EXPECT_EQ(cv2 * scm2[CVM0], cv1[CVM0]) << "mult";
        cv2.mult (cv1, scm2);
        EXPECT_EQ(cv1 * scm2(CVM0), cv2[CVM0]) << "mult";

        rv1.resize (4);
        rv2.resize (4);
        rv2.mult (rv1, srbm2);
        EXPECT_EQ(rv1 * srbm2(CVM0), rv2[CVM0]) << "mult";
        rv1.mult (srbm2, rv2);
        EXPECT_EQ(rv2 * srbm2[CVM0], rv1[CVM0]) << "mult";

        cv1.resize (4);
        cv2.resize (4);
        cv2.mult (cv1, scbm2);
        EXPECT_EQ(cv1 * scbm2(CVM0), cv2[CVM0]) << "mult";
        cv1.mult (scbm2, cv2);
        EXPECT_EQ(cv2 * scbm2[CVM0], cv1[CVM0]) << "mult";

        rm1.resize (3, 2);
        rm1[CVM0+2].assign(this->a1);
        rm3.resize (2, 2);
        rm4.resize (3, 3);
        rm3.mult (rm2, rm1);
        EXPECT_EQ(rm2[CVM0+1] * rm1(CVM0+1),rm3(CVM0+1,CVM0+1)) << "mult";
        rm4.mult (rm1, rm2);
        EXPECT_EQ(rm1[CVM0+2] * rm2(CVM0+2),rm4(CVM0+2,CVM0+2)) << "mult";
        srm4.resize(3);
        srm4.mult (rm1, rm2);

        EXPECT_EQ(rm1[CVM0+2] * rm2(CVM0+2),srm4(CVM0+2,CVM0+2)) << "mult";
        rm4.resize (3, 2);
        rm1.mult (srm4, rm4);
        EXPECT_EQ(srm4[CVM0+2] * rm4(CVM0+1),rm1(CVM0+2,CVM0+1)) << "mult";
        srbm1.resize(3);
        rm1.mult (srbm1, rm4);
        EXPECT_EQ(srbm1[CVM0+2] * rm4(CVM0+1),rm1(CVM0+2,CVM0+1)) << "mult";
        rm1.mult (~srbm1, rm4);
        EXPECT_EQ(srbm1(CVM0+2) * rm4(CVM0+1),rm1(CVM0+2,CVM0+1)) << "mult";
        srbm1.mult (rm1, rm2);
        EXPECT_EQ(rm1[CVM0+1] * rm2(CVM0+1),srbm1(CVM0+1,CVM0+1)) << "mult";

        r1 = -0.031;
        r2 = 0.319;
        rm1.randomize(1., 2.);
        rm2.randomize(0., 1.);
        rm3.randomize(0., 1.);
        rmatrix rm3_dub = rm3;

        rm3.gemm (rm2, false, rm1, false, r1, r2);
        EXPECT_EQ(0.,(rm3 - (rm2 * rm1 * r1 + rm3_dub * r2)).norm2()) << "gemm";
        rm3_dub = rm3;
        rm3 << ~rm3;
        rm3.gemm (rm1, true, rm2, true, r1, r2);
        EXPECT_EQ(0.,(~rm3 - (rm2 * rm1 * r1 + rm3_dub * r2)).norm2()) << "gemm";

        srbm1.randomize(-1., 3.);
        rmatrix rm1_dub = rm1;
        rm1.gemm (srbm1, false, rm4, false, r1, r2);
        EXPECT_EQ(0.,(rm1 - (srbm1 * rm4 * r1 + rm1_dub * r2)).norm2()) << "gemm";




        cm1.resize (3, 2);
        cm1[CVM0+2].assign(this->c1);
        cmatrix cm3 (2, 2), cm4 (3, 3);
        cm3.assign(this->c2);
        cm3.mult (cm2, cm1);
        EXPECT_EQ(cm2[CVM0+1] * cm1(CVM0+1),cm3(CVM0+1,CVM0+1)) << "mult";
        cm4.mult (cm1, cm2);
        EXPECT_EQ(cm1[CVM0+2] * cm2(CVM0+2),cm4(CVM0+2,CVM0+2)) << "mult";
        scm4.resize(3);
        scm4.mult (cm1, cm2);
        EXPECT_EQ(cm1[CVM0+2] * cm2(CVM0+2),scm4(CVM0+2,CVM0+2)) << "mult";
        cm4.resize (3, 2);
        cm1.mult (scm4, cm4);
        EXPECT_EQ(scm4[CVM0+2] * cm4(CVM0+1),cm1(CVM0+2,CVM0+1)) << "mult";
        scbm.resize(3);
        scbm.set(TPC(1.23,-0.912));
        cm1.mult (scbm, cm4);
        EXPECT_EQ(scbm[CVM0+2] * cm4(CVM0+1),cm1(CVM0+2,CVM0+1)) << "mult";
        cm1.mult (~scbm, cm4);
        EXPECT_EQ(~(scbm(CVM0+2)) * cm4(CVM0+1),cm1(CVM0+2,CVM0+1)) << "mult";
        scbm1.resize(3);
        scbm1.mult (cm1, cm2);
        EXPECT_EQ(cm1[CVM0+1] * cm2(CVM0+1),scbm1(CVM0+1,CVM0+1)) << "mult";


        cm1.randomize_real(0., 1.);
        cm2.randomize_real(0., 1.);
        cm3.randomize_real(0., 1.);
        scbm.randomize_real(0., 1.);
        cmatrix cm3_dub = cm3;
        cm3.gemm (cm2, false, cm1, false, cr1, cr2);
        EXPECT_EQ(0.,(cm3 - (cm2 * cm1 * cr1 + cm3_dub * cr2)).norm()) << "gemm";
        cmatrix cm1_dub = cm1;
        cm1.gemm (scbm, false, cm4, false, cr1, cr2);
        EXPECT_EQ(0.,(cm1 - (scbm * cm4 * cr1 + cm1_dub * cr2)).norm()) << "gemm";

        cr1 = TPC(-1.14,3.22);
        cr2 = TPC(2.04,-4.2);
        cm1_dub << cm1;
        cm1.conj();
        cm1.gemm (cm4, true, scbm, true, cr1, cr2);
        EXPECT_EQ(0.,(~cm1 - (scbm * cm4 * conj(cr1) + cm1_dub * conj(cr2))).norm2()) << "gemm";
        cm1.conj();

        rv1.randomize(0., 1.);
        rv2.randomize(0., 1.);
        EXPECT_EQ(rv1[CVM0]*rv2[CVM0]+rv1[CVM0+1]*rv2[CVM0+1]+rv1[CVM0+2]*rv2[CVM0+2]+rv1[CVM0+3]*rv2[CVM0+3], rv1 * rv2) << "scalar product";
        cv1.randomize_real(0., 1.);

        cv1.randomize_imag(0., 1.);
        cv2.randomize_real(0., 1.);
        cv2.randomize_imag(0., 1.);
        EXPECT_EQ(cv1[CVM0]*cv2[CVM0]+cv1[CVM0+1]*cv2[CVM0+1]+cv1[CVM0+2]*cv2[CVM0+2]+cv1[CVM0+3]*cv2[CVM0+3], cv1 * cv2) << "scalar product";
        EXPECT_EQ(conj(cv1[CVM0])*cv2[CVM0]+conj(cv1[CVM0+1])*cv2[CVM0+1]+conj(cv1[CVM0+2])*cv2[CVM0+2]+conj(cv1[CVM0+3])*cv2[CVM0+3], cv1 % cv2) << "scalar product, conj";

        EXPECT_EQ(0.,(rm1[CVM0+1] - (~rm1)(CVM0+1)).norm()) << "~";

        cvector cm1_2_conj (cm1[CVM0+1].size());
        cm1_2_conj = cm1[CVM0+1];
        cm1_2_conj.conj();
        EXPECT_EQ(0.,(cm1_2_conj - (~cm1)(CVM0+1)).norm()) << "~";
        EXPECT_EQ(0.,(srbm1[CVM0+1] - (~srbm1)(CVM0+1)).norm()) << "~";
        EXPECT_EQ(0.,(~(scbm1[CVM0+1]) - (~scbm1)(CVM0+1)).norm()) << "~";

        rv1.resize (3);
        rv2.resize (2);
        rv1 = rm1 * rv2;
        EXPECT_EQ(rv2 * rm1[CVM0+2], rv1[CVM0+2]) << "rmatrix * rvector";
        rv2 = rv1 * rm1;
        EXPECT_EQ(rv1 * rm1(CVM0+1), rv2[CVM0+1]) << "rvector * rmatrix";
        cv1.resize (3);
        cv2.resize (2);
        cv1 = cm1 * cv2;
        EXPECT_EQ(cv2 * cm1[CVM0+2], cv1[CVM0+2]) << "cmatrix * cvector";
        cv2 = cv1 * cm1;
        EXPECT_EQ(cv1 * cm1(CVM0+1), cv2[CVM0+1]) << "cvector * cmatrix";

        rv2.resize (3);
        rv2 = srm4 * rv1;
        EXPECT_EQ(rv1 * srm4[CVM0+2], rv2[CVM0+2]) << "srmatrix * rvector";
        rv2 = rv1 * srm4;
        EXPECT_EQ(rv1 * srm4(CVM0+2), rv2[CVM0+2]) << "rvector * srmatrix";
        cv2.resize (3);
        cv2 = scm4 * cv1;
        EXPECT_EQ(cv1 * scm4[CVM0+2], cv2[CVM0+2]) << "scmatrix * cvector";
        cv2 = cv1 * scm4;
        EXPECT_EQ(cv1 * scm4(CVM0+2), cv2[CVM0+2]) << "cvector * scmatrix";

        srbm1.normalize();
        rv1.normalize();
        rv2 = srbm1 * rv1;
        EXPECT_EQ(rv1 * srbm1[CVM0+2], rv2[CVM0+2]) << "srbmatrix * rvector";
        rv2 = rv1 * srbm1;
        EXPECT_EQ(rv1 * srbm1(CVM0+2), rv2[CVM0+2]) << "rvector * srbmatrix";

        scbm1.normalize();
        cv1.normalize();
        cv2 = scbm1 * cv1;
        EXPECT_EQ(cv1 * scbm1[CVM0+2], cv2[CVM0+2]) << "scbmatrix * cvector";
        cv2 = cv1 * scbm1;
        EXPECT_EQ(cv1 * scbm1(CVM0+2), cv2[CVM0+2]) << "cvector * scbmatrix";

        rv2.resize (2);
        rm1 = rv1.rank1update (rv2);
        EXPECT_EQ(rv1[CVM0+2] * rv2[CVM0],rm1(CVM0+2,CVM0)) << "rank1update";
        rm1.rank1update (rv1, rv2);
        EXPECT_EQ(rv1[CVM0+2] * rv2[CVM0+1],rm1(CVM0+2,CVM0+1)) << "rank1update";

        cv2.resize (2);
        cv1.normalize();
        cv2.normalize();
        cm1 = cv1.rank1update_u (cv2);
        EXPECT_EQ(cv1[CVM0+2] * cv2[CVM0],cm1(CVM0+2,CVM0)) << "rank1update_u";
        cm1.rank1update_u (cv1, cv2);
        EXPECT_EQ(cv1[CVM0+2] * cv2[CVM0+1],cm1(CVM0+2,CVM0+1)) << "rank1update_u";
        cm1 = cv1.rank1update_c (cv2);
        EXPECT_EQ(cv1[CVM0+2] * conj (cv2[CVM0]),cm1(CVM0+2,CVM0)) << "rank1update_c";
        cm1.rank1update_c (cv1, cv2);
        EXPECT_EQ(cv1[CVM0+2] * conj (cv2[CVM0+1]),cm1(CVM0+2,CVM0+1)) << "rank1update_c";

        srm4.assign(this->a3);
        srm4(CVM0+2, CVM0+2) = -1.;
        srm4.normalize();
        EXPECT_EQ(1. / (srm4.norminf() * srm4.inv().norminf()),srm4.cond()) << "cond";
        TP dt = srm4.det();
        EXPECT_EQ(srm4(CVM0,CVM0) * srm4(CVM0+1, CVM0+1) * srm4(CVM0+2, CVM0+2) -
                  srm4(CVM0, CVM0) * srm4(CVM0+1,CVM0+2) * srm4(CVM0+2, CVM0+1) -
                  srm4(CVM0, CVM0+1) * srm4(CVM0+1,CVM0) * srm4(CVM0+2, CVM0+2) +
                  srm4(CVM0, CVM0+1) * srm4(CVM0+1,CVM0+2) * srm4(CVM0+2, CVM0) +
                  srm4(CVM0, CVM0+2) * srm4(CVM0+1,CVM0) * srm4(CVM0+2, CVM0+1) -
                  srm4(CVM0, CVM0+2) * srm4(CVM0+1,CVM0+1) * srm4(CVM0+2, CVM0), dt) << "det";

        scm4.assign(this->c2);
        scm4.normalize();
        EXPECT_EQ(1. / (scm4.norminf() * scm4.inv().norminf()),scm4.cond()) << "cond";
        TPC dtc = scm4.det();
        EXPECT_EQ(scm4(CVM0,CVM0) * scm4(CVM0+1, CVM0+1) * scm4(CVM0+2, CVM0+2) -
                                  scm4(CVM0, CVM0) * scm4(CVM0+1,CVM0+2) * scm4(CVM0+2, CVM0+1) -
                                  scm4(CVM0, CVM0+1) * scm4(CVM0+1,CVM0) * scm4(CVM0+2, CVM0+2) +
                                  scm4(CVM0, CVM0+1) * scm4(CVM0+1,CVM0+2) * scm4(CVM0+2, CVM0) +
                                  scm4(CVM0, CVM0+2) * scm4(CVM0+1,CVM0) * scm4(CVM0+2, CVM0+1) -
                                  scm4(CVM0, CVM0+2) * scm4(CVM0+1,CVM0+1) * scm4(CVM0+2, CVM0), dtc) << "complex det";


        r1 = 2.;
        rv1.resize (4);
        rm1.resize (4, 4);
        srbm1.resize (4);
        rv1.set(1.);
        rm1 << eye_real(4);
        srm4 << eye_real(4);
        srbm1 << srbmatrix (eye_real(4), 0, 0);

        EXPECT_EQ(r1,rv1.norm()) << "rvector norm";
        EXPECT_EQ(r1,rm1.norm()) << "rmatrix norm";
        EXPECT_EQ(r1,srm4.norm()) << "srmatrix norm";
        EXPECT_EQ(r1,srbm1.norm()) << "srbmatrix norm";

        r1 = 2. * sqrt (2.);
        cv1.resize (4);


        cm1.resize (4, 4);
        scm1.resize (4);
        cv1.set(TPC(1,1));
        cm1 << scmatrix (cv1);
        scm1 = cm1;
        scbm2 = scbmatrix(cm1,scbm2.lsize(),scbm2.usize());

        EXPECT_EQ(r1,cv1.norm()) << "cvector norm";
        EXPECT_EQ(r1,cm1.norm()) << "cmatrix norm";
        EXPECT_EQ(r1,scm1.norm()) << "scmatrix norm";
        EXPECT_EQ(r1,scbm2.norm()) << "scbmatrix norm";

        // mix
        scbm2.set(TPC(1.23,-0.977));

        cm1 = scbm2;
        EXPECT_EQ(scbm2(CVM0+1,CVM0+2), cm1(CVM0+1,CVM0+2)) << "mix cmatrix  scbm";
        EXPECT_EQ(scbm2(CVM0+3,CVM0), cm1(CVM0+3,CVM0)) << "mix cmatrix  scbm";

        cm1 = cm1 + scbm2;
        cm1 += scbm2;
        EXPECT_EQ(scbm2(CVM0+1,CVM0+2) * 3, cm1(CVM0+1,CVM0+2)) << "mix cmatrix  scbm";
        EXPECT_EQ(scbm2(CVM0+3,CVM0) * 3., cm1(CVM0+3,CVM0)) << "mix cmatrix  scbm";
        EXPECT_EQ(3 * scbm2(CVM0+1,CVM0+2), cm1(CVM0,CVM0+1)) << "mix cmatrix  scbm";
        EXPECT_EQ(3. * scbm2(CVM0+1,CVM0), cm1(CVM0+1,CVM0)) << "mix cmatrix  scbm";

        rm1 = srbm2;
        EXPECT_EQ(srbm2(CVM0+1,CVM0+2), rm1(CVM0+1,CVM0+2)) << "mix rmatrix  srbm";
        EXPECT_EQ(srbm2(CVM0+3,CVM0), rm1(CVM0+3,CVM0)) << "mix rmatrix  srbm";

        rm1 = rm1 + srbm2;
        rm1 += srbm2;
        EXPECT_EQ(srbm2(CVM0+1,CVM0+2) * 3., rm1(CVM0+1,CVM0+2)) << "mix rmatrix  srbm";
        EXPECT_EQ(3. * srbm2(CVM0+3,CVM0), rm1(CVM0+3,CVM0)) << "mix matrix  srbm";
        EXPECT_EQ(3 * srbm2(CVM0+3,CVM0), rm1(CVM0+3,CVM0)) << "mix rmatrix  srbm";
        EXPECT_EQ(srbm2(CVM0+1,CVM0+2) * 3, rm1(CVM0+1,CVM0+2)) << "mix rmatrix  srbm";

        scbm1.resize(4);
        for (int j = CVM0; j <= CVM0+3; j++) {
            for (int i = CVM0; i <= CVM0+3; i++) {
                rm1(i,j)  = - ((j - CVM0) * 4 + i + (1 - CVM0));
                srm4(i,j) = - ((j - CVM0) * 4 + i + (1 - CVM0));
                cm1(i,j)  = - ((j - CVM0) * 4 + i + (1 - CVM0));
                scm1(i,j) = - ((j - CVM0) * 4 + i + (1 - CVM0));
            }
            srbm1(j,j) = TP(j + (1 - CVM0));
            scbm1(j,j) = TPC(j + (1 - CVM0));
        }

        EXPECT_EQ((13 + 14 + 15 + 16),rm1.norm1()) << "rmatrix norm1";
        EXPECT_EQ((13 + 14 + 15 + 16),srm4.norm1()) << "srmatrix norm1";
        EXPECT_EQ(4,srbm1.norm1()) << "srbmatrix norm1";
        EXPECT_EQ((13 + 14 + 15 + 16),cm1.norm1()) << "cmatrix norm1";
        EXPECT_EQ((13 + 14 + 15 + 16),scm1.norm1()) << "scmatrix norm1";
        EXPECT_EQ(4,scbm1.norm1()) << "scbmatrix norm1";

        EXPECT_EQ((4 + 8 + 12 + 16),rm1.norminf()) << "rmatrix norminf";
        EXPECT_EQ((4 + 8 + 12 + 16),srm4.norminf()) << "srmatrix norminf";
        EXPECT_EQ(4,srbm1.norminf()) << "srbmatrix norminf";
        EXPECT_EQ((4 + 8 + 12 + 16),cm1.norminf()) << "cmatrix norminf";
        EXPECT_EQ((4 + 8 + 12 + 16),scm1.norminf()) << "scmatrix norminf";
        EXPECT_EQ(4,scbm1.norminf()) << "scbmatrix norminf";

        EXPECT_EQ(1.,eye_real(6)(CVM0+5,CVM0+5)) << "eye_real";
        EXPECT_EQ(TPC(1,0),eye_complex(6)(CVM0+5,CVM0+5)) << "eye_complex";

        rv2.resize (4);
        srmatrix rmU(4), rmVH(4);
        rv1 = srm4.svd (rmU, rmVH);
        rv2.svd (srm4, rmU, rmVH);
//        EXPECT_TRUE(rv1 == rv2) << "srmatrix svd";
        EXPECT_EQ(0.,(rv1 - rv2).norm()) << "srmatrix svd";
        srm1 << srmatrix (rv1);
        EXPECT_EQ(0.,(srm4 * ~rmVH - rmU * srm1).norm()) << "srmatrix svd";
        EXPECT_EQ(0.,(~srm4 * rmU - ~(srm1 * rmVH)).norm()) << "srmatrix svd";

        rv1 = srbm2.svd (rmU, rmVH);
        rv2.svd (srbm2);

        EXPECT_EQ(0.,(rv1 - rv2).norm()) << "srbmatrix svd";
        rv2.svd (srbm2, rmU, rmVH);
        srm1 << srmatrix (rv1);
        EXPECT_EQ(0.,(srbm2 * ~rmVH - rmU * srm1).norm()) << "srbmatrix svd";
        EXPECT_EQ(0.,(~srbm2 * rmU - ~(srm1 * rmVH)).norm()) << "srbmatrix svd";





}

TYPED_TEST(InitializationTest, TestSubAssignment) {
    basic_rvector<TP> rv(10), rv2(4);
    rv2.randomize(-3., 2.);
    rv.assign(CVM0+2, rv2);
    EXPECT_EQ(rv[CVM0+2], rv2[CVM0]) << "rvector subvector assignment";
    EXPECT_EQ(rv[CVM0+5], rv2[CVM0+3]) << "rvector subvector assignment";

    basic_cvector<TP,TPC> cv(10), cv2(4);
    cv2.randomize_real(-3., 2.);
    cv2.randomize_imag(-3., 2.);
    cv.assign(CVM0+2, cv2);
    EXPECT_EQ(cv[CVM0+2], cv2[CVM0]) << "cvector subvector assignment";
    EXPECT_EQ(cv[CVM0+5], cv2[CVM0+3]) << "cvector subvector assignment";

    basic_rmatrix<TP> rm(5,5), rm2(6,6);
    rm.randomize(-3., 2.);
    rm2.assign(CVM0+1, CVM0+1, rm);
    EXPECT_EQ(rm(CVM0,CVM0), rm2(CVM0+1,CVM0+1)) <<  "rmatrix submatrix assignment";
    EXPECT_EQ(rm(CVM0+1,CVM0+2), rm2(CVM0+2,CVM0+3)) <<  "rmatrix submatrix assignment";

    basic_srmatrix<TP> srm(6);
    srm.randomize(-3., 2.);
    srm.assign(CVM0+1, CVM0+1, rm);
    EXPECT_EQ(rm(CVM0,CVM0), srm(CVM0+1,CVM0+1)) <<  "srmatrix submatrix assignment";
    EXPECT_EQ(rm(CVM0+1,CVM0+2), srm(CVM0+2,CVM0+3)) <<  "srmatrix submatrix assignment";

    basic_cmatrix<TP,TPC> cm(5,5), cm2(6,6);
    cm.randomize_real(-3., 2.);
    cm.randomize_imag(-3., 2.);
    cm2.assign(CVM0+1, CVM0+1, cm);
    EXPECT_EQ(cm(CVM0,CVM0), cm2(CVM0+1,CVM0+1)) <<  "cmatrix submatrix assignment";
    EXPECT_EQ(cm(CVM0+1,CVM0+2), cm2(CVM0+2,CVM0+3)) <<  "cmatrix submatrix assignment";

    basic_scmatrix<TP,TPC> scm(6);
    scm.randomize_real(-3., 2.);
    scm.randomize_imag(-3., 2.);
    scm.assign(CVM0+1, CVM0+1, cm);
    EXPECT_EQ(cm(CVM0,CVM0), scm(CVM0+1,CVM0+1)) <<  "scmatrix submatrix assignment";
    EXPECT_EQ(cm(CVM0+1,CVM0+2), scm(CVM0+2,CVM0+3)) <<  "scmatrix submatrix assignment";

    basic_srsmatrix<TP> srs1(5), srs2(3);
    tint ns = srs1.msize();
    srs1.resize(5);
    srs2.randomize(-3., 2.);
    srs1.assign(CVM0+2,srs2);
    EXPECT_EQ(srs2(CVM0,CVM0), srs1(CVM0+2,CVM0+2)) <<  "srsmatrix submatrix assignment";
    EXPECT_EQ(srs2(CVM0+1,CVM0+2), srs1(CVM0+3,CVM0+4)) <<  "srsmatrix submatrix assignment";
    srs1.resize(ns);

    basic_schmatrix<TP,TPC> sch1(1), sch2(3);
    ns = sch1.msize();
    sch1.resize(5);
    sch2.randomize_real(-3., 2.);
    sch2.randomize_imag(-3., 2.);
    sch1.assign(CVM0+2,sch2);
    EXPECT_EQ(sch2(CVM0,CVM0), sch1(CVM0+2,CVM0+2)) <<  "schmatrix submatrix assignment";
    EXPECT_EQ(sch2(CVM0+1,CVM0+2), sch1(CVM0+3,CVM0+4)) <<  "schmatrix submatrix assignment";
    sch1.resize(ns);
}
