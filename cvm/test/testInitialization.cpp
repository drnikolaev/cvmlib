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
    EXPECT_NEAR(rv(CVM0 + 2), 3.456, s<TP>());
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

