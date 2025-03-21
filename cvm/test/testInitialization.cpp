//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2022
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "test.h"

template <typename T>
class InitializationTest : public ::testing::Test {
protected:
    InitializationTest() :
    cs({{3., 0., 2., 1., -1., 2., 2., -1., 3., 0., 0., 3., -1., -2., 0., -3., 5., 0.}}),
    as({{1., 2., 1., 2., 5., -1., 1., -1., 20.}})
    {
        for (int i = 0; i < 100; ++i)
        {
            a1[i] = T(i + 1);
            a2[i] = T(i + 1) / T(10.);
            a3[i] = T(i + 1) * T(10.);
            a4[i] = T(i + 1) / T(100.);
            c1[i] = std::complex<T>(a1[i], a2[i]);
            c2[i] = std::complex<T>(a2[i], a4[i]);
        }
    }
    virtual ~InitializationTest() {}

    iarray ia;
    basic_rvector<T> rv;
    basic_rmatrix<T> rm;
    basic_srmatrix<T> srm;
    basic_srbmatrix<T> srbm;
    basic_srsmatrix<T> srsm;
    basic_cvector<T,std::complex<T>> cv;
    basic_cmatrix<T,std::complex<T>> cm;
    basic_scmatrix<T,std::complex<T>> scm;
    basic_scbmatrix<T,std::complex<T>> scbm;
    basic_schmatrix<T,std::complex<T>> schm;

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
    std::complex<T> c1[100], c2[100];

    const std::array<T,18> cs;
    const std::array<T,9> as;
};

TYPED_TEST_SUITE(InitializationTest, TestTypes);

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

TYPED_TEST(InitializationTest, TestInitList) {
    basic_rvector<TP> rv = { TP(1.), TP(-2.), TP(3.456), TP(99.99) };
    basic_rvector<TP> rv0 = {};
    basic_cvector<TP,TPC> cv = { TPC(1.2, 3.4), TPC(3.4, 5.6), TP(99.99) };
    basic_rvector<TP> cv0 = {};
    EXPECT_EQ(0,rv0.size());
    EXPECT_EQ(0,cv0.size());
    EXPECT_EQ(1.,rv[0]);
    EXPECT_EQ(TP(3.456), rv(2));
    EXPECT_EQ(TPC(1.2, 3.4),cv(0));
    EXPECT_EQ(TPC(99.99, 0.),cv(2));
}

TYPED_TEST(InitializationTest, TestLiterals) {
//    _Complex float cc = 3.4f + 5.6fi;
    std::complex<float> c = 3.4f + 5.6if;
    EXPECT_EQ(std::complex<float>(3.4, 5.6), c);

    const cvector64 vc = { 2.i, -2.i, 2.1i, -2.1i,
        1 + 2.i, 1.1 + 2.i, 1 + 2.1i, 1.1 + 2.1i,
        2.i + 4, 2.i + 4.1, 2.1i + 4, 2.1i + 4.1,
        1 - 2.i, 1.1 - 2.i, 1 - 2.1i, 1.1 - 2.1i,
        2.i - 4, 2.i - 4.1, 2.1i - 4, 2.1i - 4.1 };
    EXPECT_NEAR(vc.norm(), 1.4972641717479251e+01, sp<double>());
}

TYPED_TEST(InitializationTest, TestIarrayInsertErase) {
    iarray a(5);
    iarray::iterator pos = a.begin() + 2;
    a.insert(pos, 88);
    EXPECT_EQ(88, a[2]) << "iarray::begin, iarray::insert";
    pos = a.begin() + 1;
    a.erase(pos);
    EXPECT_EQ(88, a[1]) << "iarray::begin, iarray::erase";
    EXPECT_EQ(0, a[2]) << "iarray::begin, iarray::erase";
}

TYPED_TEST(InitializationTest, TestIarrayPushBack) {
    iarray a(5);
    a.push_back(88);
    EXPECT_EQ(88, a[5]) << "iarray::push_back";
    a.pop_back();
    EXPECT_EQ(0, a[4]) << "iarray::pop_back";
    EXPECT_EQ(5, a.size()) << "iarray::pop_back";
}

TYPED_TEST(InitializationTest, TestIarrayAt) {
    iarray a(5);
    a[0] = 1;
    a[1] = 2;
    a[2] = 3;
    a[3] = 4;
    a[4] = 5;
    EXPECT_EQ(1, a.at(0)) << "iarray::at";
    EXPECT_EQ(5, a.at(4)) << "iarray::at";
}

TYPED_TEST(InitializationTest, TestIarrayReverseIteratorFrontBack) {
    iarray a(5);
    a[0] = 1;
    a[1] = 2;
    a[2] = 3;
    a[3] = 4;
    a[4] = 5;
    
    int val = 5;
    for (iarray::reverse_iterator it = a.rbegin(); it != a.rend(); ++it)
    {
        EXPECT_EQ(val, *it) << "iarray::reverse_iterator";
        --val;
    }
    EXPECT_EQ(1, a.front()) << "iarray::front";
    EXPECT_EQ(5, a.back()) << "iarray::back";
}

TYPED_TEST(InitializationTest, TestIarrayResize) {
    const tint a[] = {1, 2, 3, 4};
    iarray v(a, 3);
    v.resize(2);
    EXPECT_EQ(2, v[1]) << "iarray.resize";
    v.resize(4);
    EXPECT_EQ(0, v[3]) << "iarray.resize";
}

TYPED_TEST(InitializationTest, TestIarrayCopyCtr) {
    iarray a(5);
    a.set(3);
    iarray b(a);
    EXPECT_EQ(3, b[3]) << "iarray copy ctr";
}

TYPED_TEST(InitializationTest, TestIarrayAssignment) {
    iarray a(5), b(5);
    a.set(3);
    b = a;
    EXPECT_EQ(3, b[3]) << "iarray assignment";
}

TYPED_TEST(InitializationTest, TestIarraySet) {
    iarray a(5);
    a.set(3);
    EXPECT_EQ(3, a[3]) << "iarray.set";
}

TYPED_TEST(InitializationTest, TestIarrayResize2) {
    iarray a;
    a.resize(10);
    EXPECT_EQ(10, a.size()) << "iarray.resize";
}

TYPED_TEST(InitializationTest, TestIarrayArrCtr) {
    tint a[] = {1, 2, 3, 4};
    iarray v(a, 3);
    EXPECT_EQ(2, v[1]) << "iarray(*, size)";
    a[1] = 77;
    EXPECT_EQ(77, v[1]) << "iarray(*, size)";
}

TYPED_TEST(InitializationTest, TestIarrayItrCtr) {
    const tint a[] = {1, 2, 3, 4};
    const iarray v{a+1, a+3};
    EXPECT_EQ(2, v.size()) << "iarray.size()";
    EXPECT_EQ(3, v[1]) << "iarray(*, *)";
}

TYPED_TEST(InitializationTest, TestIarrayGet) {
    iarray a(10);
    a[1] = 1;
    EXPECT_EQ(1, a.get()[1]) << "iarray.get";
}

TYPED_TEST(InitializationTest, TestVectorCopyCtrReal) {
    basic_rvector<TP> a(5);
    a.set(3.);
    basic_rvector<TP> b(a);
    EXPECT_EQ(TP(3.), b[3]) << "rvector copy ctr";
}

TYPED_TEST(InitializationTest, TestVectorItr) {
    basic_rvector<TP> vs1(5);
    vs1[0] = 1.; vs1[1] = 2.; vs1[2] = 3.; vs1[3] = 4.; vs1[4] = 5.;
    
    typename basic_rvector<TP>::iterator it = vs1.begin() + 1;
    typename basic_rvector<TP>::iterator ite = vs1.erase(it);
    
    EXPECT_EQ(1. , vs1[0]) << "rvector.insert";
    EXPECT_EQ(3. , vs1[1]) << "rvector.insert";
    EXPECT_EQ(4. , vs1[2]) << "rvector.insert";
    
    vs1.insert(ite, 10.);
    
    EXPECT_EQ(1. , vs1[0]) << "rvector.insert";
    EXPECT_EQ(10. , vs1[1]) << "rvector.insert";
    EXPECT_EQ(3. , vs1[2]) << "rvector.insert";
    
    vs1.push_back(9.);
    EXPECT_EQ(5. , vs1[4]) << "rvector.push_back";
    EXPECT_EQ(9. , vs1[5]) << "rvector.push_back";
    EXPECT_EQ(10. , *std::max_element(vs1.begin(), vs1.end())) << "rvector.max_element";
    
    std::sort(vs1.begin(), vs1.end());
    EXPECT_EQ(10. , vs1[5]) << "std::sort";
    
    std::reverse(vs1.begin(), vs1.end());
    EXPECT_EQ(10. , vs1[0]) << "std::reverse";
    EXPECT_EQ(9. , vs1[1]) << "std::reverse";
    EXPECT_EQ(1. , vs1[5]) << "std::reverse";
}

TYPED_TEST(InitializationTest, TestSVDbugfixNgt64M) {
    // N > 64*M bug fixed
    basic_rmatrix<TP> A(600, 4);
    basic_srmatrix<TP> U(600), V(4);
    const basic_rvector<TP> singVal = A.svd(U, V);
    basic_rmatrix<TP> singValM{A.msize(), A.nsize()};
    singValM.diag(0) = singVal;
    EXPECT_NEAR(TP(0.), (A * ~V - U * singValM).norm(), sf<TP>()) << "rmatrix svd";
    EXPECT_NEAR(TP(0.), (~A * U - ~(singValM * V)).norm(), sf<TP>()) << "rmatrix svd";
}

TYPED_TEST(InitializationTest, TestMoveReal) {
    basic_rmatrix<TP> rm{4,3};
    rm.set(2.);
    TP ar[] = {1., 2., 3., 4., 5.};
    basic_rvector<TP> a{ar, 3, 2}, b(3), c(3), aa(3);
    b[0] = 3;
    c[0] = 4;
    rm[1] = a + c;

    aa = a; // copy from sparse
    EXPECT_EQ(1.,aa(0));
    EXPECT_EQ(3.,aa(1));
    EXPECT_EQ(5.,aa(2));

    EXPECT_EQ(2., rm(0,0));
    EXPECT_EQ(5.,rm(1,0));
    EXPECT_EQ(2.,rm(2,0));
    EXPECT_EQ(2.,rm(0,1));
    EXPECT_EQ(3.,rm(1,1));
    EXPECT_EQ(5.,rm(1,2));
    EXPECT_EQ(2.,rm(3,2));

    basic_rvector<TP> d = a + b + c;
    EXPECT_EQ(8.,d(0));
    EXPECT_EQ(3.,d(1));
    EXPECT_EQ(5.,d(2));

    basic_rvector<TP> e = a + b;
    EXPECT_EQ(4.,e(0));
    EXPECT_EQ(3.,e(1));
    EXPECT_EQ(5.,e(2));

    iarray ia(3), ib(3);
    ib[0] = 4;
    ia = std::move(ib);
    EXPECT_EQ(4,ia(0));
    EXPECT_EQ(0,ia(1));
    EXPECT_EQ(0,ia(2));
//  EXPECT_EQ(ib.size(), 0);

    basic_rvector<TP> f(4);
    {
        basic_rmatrix<TP> rm{4,3};
        rm.set(9.);
        f = rm(0);
        basic_rvector<TP> g = std::move(rm[1]);
        EXPECT_EQ(g(0), 9.);
        EXPECT_EQ(g(1), 9.);
        EXPECT_EQ(g(2), 9.);
    }
    EXPECT_EQ(9.,f(0));
    EXPECT_EQ(9.,f(1));
    EXPECT_EQ(9.,f(2));
    EXPECT_EQ(9.,f(3));
}

TYPED_TEST(InitializationTest, TestMoveComplex) {
    basic_cmatrix<TP,TPC> rm{4,3};
    rm.set(2.);
    TP ar[] = {1., 2., 3., 4., 5., 1., 2., 3., 4., 5., -1., -2.};
    basic_cvector<TP,TPC> a{(TPC*)ar, 3, 2}, b(3), c(3), aa(3);
    b[0] = TPC(3,3);
    c[0] = TPC(4,4);
    rm[1] = a + c;

    aa = a; // copy from sparse
    EXPECT_EQ(aa(0),TPC(1.,2.));
    EXPECT_EQ(TPC(5.,1.),aa(1));
    EXPECT_EQ(TPC(4.,5.),aa(2));

    EXPECT_EQ(TPC(2.,0.),rm(0,0));
    EXPECT_EQ(TPC(5.,6.),rm(1,0));
    EXPECT_EQ(TPC(2.,0.),rm(2,0));
    EXPECT_EQ(TPC(2.,0.),rm(0,1));
    EXPECT_EQ(TPC(5.,1.),rm(1,1));
    EXPECT_EQ(TPC(4.,5.),rm(1,2));
    EXPECT_EQ(TPC(2.,0.),rm(3,2));

    basic_cvector<TP,TPC> e = a + b;
    EXPECT_EQ(TPC(4.,5.),e(0));
    EXPECT_EQ(TPC(5.,1.),e(1));
    EXPECT_EQ(TPC(4.,5.),e(2));

    basic_cvector<TP,TPC> f(4);
    {
        basic_cmatrix<TP,TPC> cm{4,3};
        cm.set(TPC(2.,3.));
        f = cm(0);

        basic_cvector<TP,TPC> g = std::move(cm[1]);
        EXPECT_EQ(g(0),   TPC(2.,3.));
        EXPECT_EQ(g(1), TPC(2.,3.));
        EXPECT_EQ(g(2), TPC(2.,3.));
    }
    EXPECT_EQ(TPC(2.,3.),f(0));
    EXPECT_EQ(TPC(2.,3.),f(1));
    EXPECT_EQ(TPC(2.,3.),f(2));
    EXPECT_EQ(TPC(2.,3.),f(3));
}

TYPED_TEST(InitializationTest, TestSubmatrixMoveReal) {
    basic_rmatrix<TP> m1{2,3}, m2{2,3}, m3{2,3};
    m2.set(2.);
    m3.set(3.);
    m1 = m2 + m3;

    EXPECT_EQ(5.,m1(0,0));
    EXPECT_EQ(5.,m1(1,0));
    EXPECT_EQ(5.,m1(0,1));
    EXPECT_EQ(5.,m1(1,1));
    EXPECT_EQ(5.,m1(1,2));

    basic_rmatrix<TP> m4{m2 + m3};
    EXPECT_EQ(5.,m4(0,0));
    EXPECT_EQ(5.,m4(1,0));
    EXPECT_EQ(5.,m4(0,1));
    EXPECT_EQ(5.,m4(1,1));
    EXPECT_EQ(5.,m4(1,2));

    basic_rmatrix<TP> m7{7,5};
    m7.set(7.);
    basic_rmatrix<TP> ms{m7, 1, 2, 2, 3}; // submatrix
    basic_rmatrix<TP> mm = std::move(ms);

    EXPECT_EQ(7.,mm(0,0));
    EXPECT_EQ(7.,mm(1,0));
    EXPECT_EQ(7.,mm(0,1));
    EXPECT_EQ(7.,mm(1,1));
    EXPECT_EQ(7.,mm(1,2));

    EXPECT_EQ(7.,ms(0,0));
    EXPECT_EQ(7.,ms(1,0));
    EXPECT_EQ(7.,ms(0,1));
    EXPECT_EQ(7.,ms(1,1));
    EXPECT_EQ(7.,ms(1,2));

    ms = m2 + m3;
    EXPECT_EQ(5.,ms(0,0));
    EXPECT_EQ(5.,ms(1,0));
    EXPECT_EQ(5.,ms(0,1));
    EXPECT_EQ(5.,ms(1,1));
    EXPECT_EQ(5.,ms(1,2));

    EXPECT_EQ(7.,m7(0,2));
    EXPECT_EQ(5.,m7(1,2));
    EXPECT_EQ(5.,m7(2,2));
    EXPECT_EQ(7.,m7(3,2));
}

TYPED_TEST(InitializationTest, TestSubmatrixMoveComplex) {
    basic_cmatrix<TP,TPC> m1{2,3}, m2{2,3}, m3{2,3};
    m2.set(TPC(2.,2.));
    m3.set(TPC(3.,3.));
    m1 = m2 + m3;

    EXPECT_EQ(TPC(5.,5.),m1(0,0));
    EXPECT_EQ(TPC(5.,5.),m1(1,0));
    EXPECT_EQ(TPC(5.,5.),m1(0,1));
    EXPECT_EQ(TPC(5.,5.),m1(1,1));
    EXPECT_EQ(TPC(5.,5.),m1(1,2));

    basic_cmatrix<TP,TPC> m4{m2 + m3};
    EXPECT_EQ(TPC(5.,5.),m4(0,0));
    EXPECT_EQ(TPC(5.,5.),m4(1,0));
    EXPECT_EQ(TPC(5.,5.),m4(0,1));
    EXPECT_EQ(TPC(5.,5.),m4(1,1));
    EXPECT_EQ(TPC(5.,5.),m4(1,2));

    basic_cmatrix<TP,TPC> m7{7,5};
    m7.set(TPC(7.,7.));
    basic_cmatrix<TP,TPC> ms{m7, 1, 2, 2, 3}; // submatrix
    basic_cmatrix<TP,TPC> mm = std::move(ms);

    EXPECT_EQ(TPC(7.,7.),mm(0,0));
    EXPECT_EQ(TPC(7.,7.),mm(1,0));
    EXPECT_EQ(TPC(7.,7.),mm(0,1));
    EXPECT_EQ(TPC(7.,7.),mm(1,1));
    EXPECT_EQ(TPC(7.,7.),mm(1,2));

    EXPECT_EQ(TPC(7.,7.),ms(0,0));
    EXPECT_EQ(TPC(7.,7.),ms(1,0));
    EXPECT_EQ(TPC(7.,7.),ms(0,1));
    EXPECT_EQ(TPC(7.,7.),ms(1,1));
    EXPECT_EQ(TPC(7.,7.),ms(1,2));

    ms = m2 + m3;
    EXPECT_EQ(TPC(5.,5.),ms(0,0));
    EXPECT_EQ(TPC(5.,5.),ms(1,0));
    EXPECT_EQ(TPC(5.,5.),ms(0,1));
    EXPECT_EQ(TPC(5.,5.),ms(1,1));
    EXPECT_EQ(TPC(5.,5.),ms(1,2));

    EXPECT_EQ(TPC(7.,7.),m7(0,2));
    EXPECT_EQ(TPC(5.,5.),m7(1,2));
    EXPECT_EQ(TPC(5.,5.),m7(2,2));
    EXPECT_EQ(TPC(7.,7.),m7(3,2));
}

TYPED_TEST(InitializationTest, TestSquareSubmatrixMoveReal) {
    basic_srmatrix<TP> m1{3}, m2{3}, m3{3};
    m2.set(2.);
    m3.set(3.);
    m1 = m2 + m3;

    EXPECT_EQ(5.,m1(0,0));
    EXPECT_EQ(5.,m1(1,0));
    EXPECT_EQ(5.,m1(0,1));
    EXPECT_EQ(5.,m1(1,1));
    EXPECT_EQ(5.,m1(1,2));

    basic_srmatrix<TP> m4{m2 + m3};
    EXPECT_EQ(5.,m4(0,0));
    EXPECT_EQ(5.,m4(1,0));
    EXPECT_EQ(5.,m4(0,1));
    EXPECT_EQ(5.,m4(1,1));
    EXPECT_EQ(5.,m4(1,2));

    basic_srmatrix<TP> m7{7};
    m7.set(7.);
    basic_srmatrix<TP> ms{m7, 1, 2, 3}; // submatrix
    basic_srmatrix<TP> mm = std::move(ms);

    EXPECT_EQ(7.,mm(0,0));
    EXPECT_EQ(7.,mm(1,0));
    EXPECT_EQ(7.,mm(0,1));
    EXPECT_EQ(7.,mm(1,1));
    EXPECT_EQ(7.,mm(1,2));

    EXPECT_EQ(7.,ms(0,0));
    EXPECT_EQ(7.,ms(1,0));
    EXPECT_EQ(7.,ms(0,1));
    EXPECT_EQ(7.,ms(1,1));
    EXPECT_EQ(7.,ms(1,2));

    ms = m2 + m3;
    EXPECT_EQ(5.,ms(0,0));
    EXPECT_EQ(5.,ms(1,0));
    EXPECT_EQ(5.,ms(0,1));
    EXPECT_EQ(5.,ms(1,1));
    EXPECT_EQ(5.,ms(1,2));

    EXPECT_EQ(7.,m7(0,2));
    EXPECT_EQ(5.,m7(1,2));
    EXPECT_EQ(5.,m7(2,2));
    EXPECT_EQ(5.,m7(3,2));
    EXPECT_EQ(7.,m7(4,2));
}

TYPED_TEST(InitializationTest, TestSquareSubmatrixMoveComplex) {
    basic_scmatrix<TP,TPC> m1{3}, m2{3}, m3{3};
    m2.set(TPC(2.,2.));
    m3.set(TPC(3.,3.));
    m1 = m2 + m3;

    EXPECT_EQ(TPC(5.,5.),m1(0,0));
    EXPECT_EQ(TPC(5.,5.),m1(1,0));
    EXPECT_EQ(TPC(5.,5.),m1(0,1));
    EXPECT_EQ(TPC(5.,5.),m1(1,1));
    EXPECT_EQ(TPC(5.,5.),m1(1,2));

    basic_scmatrix<TP,TPC> m4{m2 + m3};
    EXPECT_EQ(TPC(5.,5.),m4(0,0));
    EXPECT_EQ(TPC(5.,5.),m4(1,0));
    EXPECT_EQ(TPC(5.,5.),m4(0,1));
    EXPECT_EQ(TPC(5.,5.),m4(1,1));
    EXPECT_EQ(TPC(5.,5.),m4(1,2));

    basic_scmatrix<TP,TPC> m7{7};
    m7.set(TPC(7.,7.));
    basic_scmatrix<TP,TPC> ms{m7, 1, 2, 3}; // submatrix
    basic_scmatrix<TP,TPC> mm = std::move(ms);

    EXPECT_EQ(TPC(7.,7.),mm(0,0));
    EXPECT_EQ(TPC(7.,7.),mm(1,0));
    EXPECT_EQ(TPC(7.,7.),mm(0,1));
    EXPECT_EQ(TPC(7.,7.),mm(1,1));
    EXPECT_EQ(TPC(7.,7.),mm(1,2));

    EXPECT_EQ(TPC(7.,7.),ms(0,0));
    EXPECT_EQ(TPC(7.,7.),ms(1,0));
    EXPECT_EQ(TPC(7.,7.),ms(0,1));
    EXPECT_EQ(TPC(7.,7.),ms(1,1));
    EXPECT_EQ(TPC(7.,7.),ms(1,2));

    ms = m2 + m3;
    EXPECT_EQ(TPC(5.,5.),ms(0,0));
    EXPECT_EQ(TPC(5.,5.),ms(1,0));
    EXPECT_EQ(TPC(5.,5.),ms(0,1));
    EXPECT_EQ(TPC(5.,5.),ms(1,1));
    EXPECT_EQ(TPC(5.,5.),ms(1,2));

    EXPECT_EQ(TPC(7.,7.),m7(0,2));
    EXPECT_EQ(TPC(5.,5.),m7(1,2));
    EXPECT_EQ(TPC(5.,5.),m7(2,2));
    EXPECT_EQ(TPC(5.,5.),m7(3,2));
    EXPECT_EQ(TPC(7.,7.),m7(4,2));
}

TYPED_TEST(InitializationTest, TestBandMoveReal) {
    basic_srbmatrix<TP> m1{5,2,1}, m2{5,2,1}, m3{5,2,1};
    m2.set(2.);
    m3.set(3.);
    m1 = m2 + m3;

    EXPECT_EQ(5.,m1(0,0));
    EXPECT_EQ(5.,m1(1,0));
    EXPECT_EQ(5.,m1(0,1));
    EXPECT_EQ(5.,m1(1,1));
    EXPECT_EQ(5.,m1(1,2));

    basic_srbmatrix<TP> m4{m2 + m3};
    EXPECT_EQ(5.,m4(0,0));
    EXPECT_EQ(5.,m4(1,0));
    EXPECT_EQ(5.,m4(0,1));
    EXPECT_EQ(5.,m4(1,1));
    EXPECT_EQ(5.,m4(1,2));

    basic_srbmatrix<TP> mt{~m4};
    EXPECT_EQ(1,mt.lsize());
    EXPECT_EQ(2,mt.usize());

    EXPECT_EQ(5.,mt(1,0));
    EXPECT_EQ(0.,mt(2,0));
    EXPECT_EQ(5.,mt(0,2));
    EXPECT_EQ(0.,mt(0,3));

    basic_srmatrix<TP> ms{m1};
    EXPECT_EQ(5.,ms(0,0));
    EXPECT_EQ(5.,ms(1,0));
    EXPECT_EQ(5.,ms(0,1));
    EXPECT_EQ(5.,ms(1,1));
    EXPECT_EQ(5.,ms(1,2));
    EXPECT_EQ(0.,mt(2,0));
    EXPECT_EQ(0.,ms(1,3));
    EXPECT_EQ(0.,mt(0,3));
}

TYPED_TEST(InitializationTest, TestBandMoveComplex) {
    basic_scbmatrix<TP,TPC> m1{5,2,1}, m2{5,2,1}, m3{5,2,1};
    m2.set(TPC(2.,2.));
    m3.set(TPC(3.,3.));
    m1 = m2 + m3;

    EXPECT_EQ(TPC(5.,5.),m1(0,0));
    EXPECT_EQ(TPC(5.,5.),m1(1,0));
    EXPECT_EQ(TPC(5.,5.),m1(0,1));
    EXPECT_EQ(TPC(5.,5.),m1(1,1));
    EXPECT_EQ(TPC(5.,5.),m1(1,2));

    basic_scbmatrix<TP,TPC> m4{m2 + m3};
    EXPECT_EQ(TPC(5.,5.),m4(0,0));
    EXPECT_EQ(TPC(5.,5.),m4(1,0));
    EXPECT_EQ(TPC(5.,5.),m4(0,1));
    EXPECT_EQ(TPC(5.,5.),m4(1,1));
    EXPECT_EQ(TPC(5.,5.),m4(1,2));

    basic_scbmatrix<TP,TPC> mt{~m4};
    EXPECT_EQ(1,mt.lsize());
    EXPECT_EQ(2,mt.usize());

    EXPECT_EQ(TPC(5.,-5.),mt(1,0));
    EXPECT_EQ(TPC(0.,0.),mt(2,0));
    EXPECT_EQ(TPC(5.,-5.),mt(0,2));
    EXPECT_EQ(TPC(0.,0.),mt(0,3));

    basic_scmatrix<TP,TPC> ms{m1};
    EXPECT_EQ(TPC(5.,5.),ms(0,0));
    EXPECT_EQ(TPC(5.,5.),ms(1,0));
    EXPECT_EQ(TPC(5.,5.),ms(0,1));
    EXPECT_EQ(TPC(5.,5.),ms(1,1));
    EXPECT_EQ(TPC(5.,5.),ms(1,2));
    EXPECT_EQ(TPC(0.,0.),ms(1,3));
}

TYPED_TEST(InitializationTest, TestSymmetricSubmatrixMoveReal) {
    basic_srsmatrix<TP> m1{3}, m2{3}, m3{3};
    m2.set(2.);
    m3.set(3.);
    m1 = m2 + m3;

    EXPECT_EQ(5.,m1(0,0));
    EXPECT_EQ(5.,m1(1,0));
    EXPECT_EQ(5.,m1(0,1));
    EXPECT_EQ(5.,m1(1,1));
    EXPECT_EQ(5.,m1(1,2));

    basic_srsmatrix<TP> m4{m2 + m3};
    EXPECT_EQ(5.,m4(0,0));
    EXPECT_EQ(5.,m4(1,0));
    EXPECT_EQ(5.,m4(0,1));
    EXPECT_EQ(5.,m4(1,1));
    EXPECT_EQ(5.,m4(1,2));

    basic_srsmatrix<TP> m7{7};
    m7.set(7.);
    basic_srsmatrix<TP> ms{m7, 1, 3}; // submatrix
    basic_srsmatrix<TP> mm = std::move(ms);

    EXPECT_EQ(7.,mm(0,0));
    EXPECT_EQ(7.,mm(1,0));
    EXPECT_EQ(7.,mm(0,1));
    EXPECT_EQ(7.,mm(1,1));
    EXPECT_EQ(7.,mm(1,2));

    EXPECT_EQ(7.,ms(0,0));
    EXPECT_EQ(7.,ms(1,0));
    EXPECT_EQ(7.,ms(0,1));
    EXPECT_EQ(7.,ms(1,1));
    EXPECT_EQ(7.,ms(1,2));

    ms = m2 + m3;
    EXPECT_EQ(5.,ms(0,0));
    EXPECT_EQ(5.,ms(1,0));
    EXPECT_EQ(5.,ms(0,1));
    EXPECT_EQ(5.,ms(1,1));
    EXPECT_EQ(5.,ms(1,2));

    EXPECT_EQ(7.,m7(0,2));
    EXPECT_EQ(5.,m7(1,2));
    EXPECT_EQ(5.,m7(2,2));
    EXPECT_EQ(5.,m7(3,2));
    EXPECT_EQ(7.,m7(4,2));
}

TYPED_TEST(InitializationTest, TestHermitianSubmatrixMoveComplex) {
    basic_schmatrix<TP,TPC> m1{3}, m2{3}, m3{3};
    m2.set_real(2.);
    m3.set_real(3.);
    m1 = m2 + m3;

    EXPECT_EQ(TPC(5.,0.),m1(0,0));
    EXPECT_EQ(TPC(5.,0.),m1(1,0));
    EXPECT_EQ(TPC(5.,0.),m1(0,1));
    EXPECT_EQ(TPC(5.,0.),m1(1,1));
    EXPECT_EQ(TPC(5.,0.),m1(1,2));

    basic_schmatrix<TP,TPC> m4{m2 + m3};
    EXPECT_EQ(TPC(5.,0.),m4(0,0));
    EXPECT_EQ(TPC(5.,0.),m4(1,0));
    EXPECT_EQ(TPC(5.,0.),m4(0,1));
    EXPECT_EQ(TPC(5.,0.),m4(1,1));
    EXPECT_EQ(TPC(5.,0.),m4(1,2));

    basic_schmatrix<TP,TPC> m7{7};
    m7.set_real(7.);
    basic_schmatrix<TP,TPC> ms{m7, 1, 3}; // submatrix
    basic_schmatrix<TP,TPC> mm = std::move(ms);

    EXPECT_EQ(TPC(7.,0.),mm(0,0));
    EXPECT_EQ(TPC(7.,0.),mm(1,0));
    EXPECT_EQ(TPC(7.,0.),mm(0,1));
    EXPECT_EQ(TPC(7.,0.),mm(1,1));
    EXPECT_EQ(TPC(7.,0.),mm(1,2));

    EXPECT_EQ(TPC(7.,0.),ms(0,0));
    EXPECT_EQ(TPC(7.,0.),ms(1,0));
    EXPECT_EQ(TPC(7.,0.),ms(0,1));
    EXPECT_EQ(TPC(7.,0.),ms(1,1));
    EXPECT_EQ(TPC(7.,0.),ms(1,2));

    ms = m2 + m3;
    EXPECT_EQ(TPC(5.,0.),ms(0,0));
    EXPECT_EQ(TPC(5.,0.),ms(1,0));
    EXPECT_EQ(TPC(5.,0.),ms(0,1));
    EXPECT_EQ(TPC(5.,0.),ms(1,1));
    EXPECT_EQ(TPC(5.,0.),ms(1,2));

    EXPECT_EQ(TPC(7.,0.),m7(0,2));
    EXPECT_EQ(TPC(5.,0.),m7(1,2));
    EXPECT_EQ(TPC(5.,0.),m7(2,2));
    EXPECT_EQ(TPC(5.,0.),m7(3,2));
    EXPECT_EQ(TPC(7.,0.),m7(4,2));
}

TYPED_TEST(InitializationTest, TestConstVsNonconstforeignArray) {
    TP r[16] = {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.};
    const TP rc[16] = {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.};
    TPC c[16];
    const TPC cc[16];

    basic_srsmatrix<TP> ssr{r, 4};
    basic_srsmatrix<TP> ssrc{rc, 4};
    ssr.set(3,3,5.11);
    ssrc.set(3,3,5.11);
    EXPECT_EQ(TP(5.11), r[15]) << "basic_srsmatrix<TP>: non-const foreign array";
    EXPECT_EQ(TP(0.00), rc[15]) << "basic_srsmatrix<TP>: const foreign array";

    basic_schmatrix<TP,TPC> shc{c, 4};
    basic_schmatrix<TP,TPC> shcc{cc, 4};
    shc.set(1,1,TPC(6.11,0.));
    shcc.set(1,1,TPC(6.11,0.));
    EXPECT_EQ(TPC(6.11,0.), c[5]) << "basic_schmatrix<TP,TPC>: non-const foreign array";
    EXPECT_EQ(TPC(0.0,0.0), cc[5]) << "basic_schmatrix<TP,TPC>: const foreign array";

    basic_rvector<TP> vr{r, 16};
    basic_rvector<TP> vrc{rc, 16};
    vr[7]=3.33;
    vrc[7]=3.33;
    EXPECT_EQ(TP(3.33), r[7]) << "basic_rvector<TP>: non-const foreign array";
    EXPECT_EQ(TP(0.00), rc[7]) << "basic_rvector<TP>: const foreign array";

    basic_cvector<TP,TPC> vc{c, 16};
    basic_cvector<TP,TPC> vcc{cc, 16};
    vc[7]=TPC(3.33,4.44);
    vcc[7]=TPC(3.33,4.44);
    EXPECT_EQ(TPC(3.33,4.44), c[7]) << "basic_cvector<TP,TPC>: non-const foreign array";
    EXPECT_EQ(TPC(0.0,0.0), cc[7]) << "basic_cvector<TP,TPC>: const foreign array";

    basic_rmatrix<TP> mr{r, 3, 4};
    basic_rmatrix<TP> mrc{rc, 3, 4};
    mr(1,1)=3.22;
    mrc(1,1)=3.22;
    EXPECT_EQ(TP(3.22), r[4]) << "basic_rmatrix<TP>: non-const foreign array";
    EXPECT_EQ(TP(0.00), rc[4]) << "basic_rmatrix<TP>: const foreign array";

    basic_cmatrix<TP,TPC> mc{c, 3, 4};
    basic_cmatrix<TP,TPC> mcc{cc, 3, 4};
    mc(1,1)=TPC(3.33,4.22);
    mcc(1,1)=TPC(3.33,4.22);
    EXPECT_EQ(TPC(3.33,4.22), c[4]) << "basic_cmatrix<TP,TPC>: non-const foreign array";
    EXPECT_EQ(TPC(0.0,0.0), cc[4]) << "basic_cmatrix<TP,TPC>: const foreign array";

    basic_srmatrix<TP> sr{r, 4};
    basic_srmatrix<TP> src{rc, 4};
    sr(1,1)=3.11;
    src(1,1)=3.11;
    EXPECT_EQ(TP(3.11), r[5]) << "basic_srmatrix<TP>: non-const foreign array";
    EXPECT_EQ(TP(0.00), rc[5]) << "basic_srmatrix<TP>: const foreign array";

    basic_scmatrix<TP,TPC> sc{c, 4};
    basic_scmatrix<TP,TPC> scc{cc, 4};
    sc(1,1)=TPC(3.11,4.22);
    scc(1,1)=TPC(3.11,4.22);
    EXPECT_EQ(TPC(3.11,4.22), c[5]) << "basic_scmatrix<TP,TPC>: non-const foreign array";
    EXPECT_EQ(TPC(0.0,0.0), cc[5]) << "basic_scmatrix<TP,TPC>: const foreign array";

    basic_srbmatrix<TP> br{r, 8, 1, 0};
    basic_srbmatrix<TP> brc{rc, 8, 1, 0};
    br(1,0)=3.01;
    brc(1,0)=3.01;
    EXPECT_EQ(TP(3.01), r[1]) << "basic_srbmatrix<TP>: non-const foreign array";
    EXPECT_EQ(TP(0.00), rc[1]) << "basic_srbmatrix<TP>: const foreign array";

    basic_scbmatrix<TP,TPC> bc{c, 8, 1, 0};
    basic_scbmatrix<TP,TPC> bcc{cc, 8, 1, 0};
    bc(1,0)=TPC(3.11,2.11);
    bcc(1,0)=TPC(3.11,2.11);
    EXPECT_EQ(TPC(3.11,2.11), c[1]) << "basic_scbmatrix<TP,TPC>: non-const foreign array";
    EXPECT_EQ(TPC(0.0,0.0), cc[1]) << "basic_scbmatrix<TP,TPC>: const foreign array";
}

TYPED_TEST(InitializationTest, TestMultCrash) {
    basic_cvector<TP,TPC> cv1{this->a1, this->a2, 10};   // note: this constructor copies memory
    basic_cvector<TP,TPC> cv2{this->a1, this->a2, 10, 3};// note: this constructor copies memory
    basic_cmatrix<TP,TPC> cm1{this->a1, this->a2, 2, 3}; // note: this constructor copies memory
    basic_cmatrix<TP,TPC> cm2{cm1};
    cv1.set(TPC(2,1));
    cv2.set(TPC(-1,3));
    cm2.set(TPC(-4,3));
    cv1.resize(2);
    cv2.resize(3);
    cv2.mult(cv1, cm2);
    EXPECT_EQ(cv2[0], cv1 * cm2(0));
}

TYPED_TEST(InitializationTest, TestConstructorsAndBasicFeatures) {
    float tmp;
    basic_rvector<TP>  rv;
    basic_rvector<TP>  rv0(10);
    basic_rvector<TP>  rv1{this->a1, 10};                 // note: this constructor shares memory
    basic_rvector<TP>  rv2{this->a1, 10, 3};              // note: this constructor shares memory
    basic_rvector<TP>  rv3(11, 17.77);
    basic_rvector<TP>  rv4{rv2};                          // note: this constructor copies memory

    basic_rmatrix<TP> bigm{100, 100};
    basic_cmatrix<TP,TPC> bigcm{100, 100};
    bigm.randomize(0., 2.);
    bigcm.randomize_real(0., 2.);
    bigcm.randomize_imag(0., 2.);

    basic_rmatrix<TP>  rm;
    basic_rmatrix<TP>  rm0{bigm, 21, 34, 5, 6};
    basic_rmatrix<TP>  rm1{this->a1, 2, 3};               // note: this constructor shares memory
    basic_rmatrix<TP>  rm2{rm1};
    basic_rmatrix<TP>  rm3{rv2, true};                    // column
    basic_rmatrix<TP>  rm4{rv2, false};                   // row
    basic_srmatrix<TP> srm;
    basic_srmatrix<TP> srm0 {bigm, 43, 47, 4};
    basic_srmatrix<TP> srm1 {this->a1, 3};                // note: this constructor shares memory
    basic_srmatrix<TP> srm2 {srm1};
    basic_srmatrix<TP> srm30{srm0};

    basic_cvector<TP,TPC> cv;
    basic_cvector<TP,TPC> cv0(10);
    basic_cvector<TP,TPC> cv1 {this->a1, this->a2, 10};   // note: this constructor copies memory
    basic_cvector<TP,TPC> cv2 {this->a1, this->a2, 10, 3};// note: this constructor copies memory
    basic_cvector<TP,TPC> cv3 {this->c1, 10, 3};          // note: this constructor shares memory
    basic_cvector<TP,TPC> cv4(11, TPC(1.3, 2.4));
    basic_cvector<TP,TPC> cv5 {cv3};
    basic_cvector<TP,TPC> cv6 {rv1, rv2};                 // note: this constructor copies memory
    basic_cvector<TP,TPC> cv7 {this->a4, 10, true,  2};   // note: this constructor copies memory
    basic_cvector<TP,TPC> cv8 {this->a4, 10, false, 3};   // note: this constructor copies memory
    basic_cvector<TP,TPC> cv9 {rv2, true};
    basic_cvector<TP,TPC> cv10{rv2, false};

    basic_cmatrix<TP,TPC> cm;
    basic_cmatrix<TP,TPC> cm0{bigcm, 68, 17, 5, 6};
    basic_cmatrix<TP,TPC> cm1{this->a1, this->a2, 2, 3};  // note: this constructor copies memory
    basic_cmatrix<TP,TPC> cm2{cm1};
    basic_scmatrix<TP,TPC> scm;
    basic_scmatrix<TP,TPC> scm0{4};
    basic_scmatrix<TP,TPC> scm1{this->a1, this->a2, 3};   // note: this constructor copies memory
    basic_scmatrix<TP,TPC> scm2{scm1};

    basic_srbmatrix<TP> srbm;
    basic_srbmatrix<TP> srbm1{this->a1, 4, 1, 2};

    basic_scbmatrix<TP,TPC> scbm;
    basic_scbmatrix<TP,TPC> scbm1{this->c1, 4, 1, 2};

    basic_srbmatrix<TP> srbm5{5, 1, 2};
    basic_scbmatrix<TP,TPC> scbm5{5, 1, 2};

    basic_srsmatrix<TP> srs1{3};
    basic_schmatrix<TP,TPC> sch1{3};


// Array<TR,std::complex<T>> derived features.
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

    basic_cmatrix<TP,TPC> cm1r{rm1};
    basic_cmatrix<TP,TPC> cm1i{rm1, false};
    EXPECT_EQ(TPC(rm1(1,2),0.), cm1r[1][2]) << "basic_cmatrix<TP,TPC>(basic_rmatrix<TP>)";
    EXPECT_EQ(TPC(0.,rm1(1,2)), cm1i[1][2]) << "basic_cmatrix<TP,TPC>(basic_rmatrix<TP>)";

    rm2.resize(4, 4);
    cm2.resize(4, 4);
    basic_srmatrix<TP> srm3 {rm2};
    basic_scmatrix<TP,TPC> scm3 {cm2};

    EXPECT_EQ(16, rm2  .size());
    EXPECT_EQ(16, cm2  .size());
    EXPECT_EQ(1, rm2  .incr());
    EXPECT_EQ(1, cm2  .incr());
    EXPECT_EQ(16, srm3 .size());
    EXPECT_EQ(16, scm3 .size());
    EXPECT_EQ(1, srm3 .incr());
    EXPECT_EQ(1, scm3 .incr());

    basic_scmatrix<TP,TPC> scm4 {srm3, true};
    basic_scmatrix<TP,TPC> scm5 {srm3, false};

    EXPECT_EQ(16, scm4  .size());
    EXPECT_EQ(16, scm5  .size());
    EXPECT_EQ(1 , scm4  .incr());
    EXPECT_EQ(1 , scm5  .incr());

    basic_scmatrix<TP,TPC> scm6 {srm3, srm0};
    EXPECT_EQ(16, scm6  .size());
    EXPECT_EQ(1 , scm6  .incr());

    basic_srmatrix<TP> srm4 {srbm1};
    EXPECT_EQ(16, srm4 .size());
    EXPECT_EQ(1, srm4 .incr());

    basic_scmatrix<TP,TPC> scm8 {scbm1};
    EXPECT_EQ(16, scm8 .size());
    EXPECT_EQ(1, scm8 .incr());

    basic_srmatrix<TP> srm5 {rv2};
    EXPECT_EQ(100, srm5 .size());
    EXPECT_EQ(1, srm5 .incr());

    basic_scmatrix<TP,TPC> scm7 {cv2};
    EXPECT_EQ(100, scm7 .size());
    EXPECT_EQ(1, scm7 .incr());

// Indexing and assignments
    EXPECT_EQ(1., rv2[0]);
    EXPECT_EQ(28., rv2[9]);

    TP r1 = -1.92;
    this->a1[3] = r1;
    EXPECT_EQ(r1, rv1[3]);
    EXPECT_EQ(r1, rv2[1]);
    EXPECT_EQ(r1, rm1[1][1]);
    EXPECT_EQ(r1, rm1(1,1));
    EXPECT_EQ(r1, srm1[0][1]);
    EXPECT_EQ(r1, srm1(0, 1));

    EXPECT_NEAR(std::abs(TPC(4.,0.4)), std::abs(cm1(1, 1)), s<TP>());

    TP* pr1 = srm30;
    srm30(3,3) = r1;
    EXPECT_EQ(r1, srm30(3,3));
    EXPECT_EQ(r1, pr1[15]);

    TP* pr2 = srbm1;
    srbm1(0,1) = r1;
    EXPECT_EQ(r1, srbm1(0,1));
    EXPECT_EQ(r1, pr2[5]);

    TPC cr1 = TPC(1.07, -0.179);
    TPC* pc2 = scbm1;
    scbm1(0,1) = cr1;
    EXPECT_EQ(cr1, scbm1(0,1));
    EXPECT_EQ(cr1, pc2[5]);

    srs1.assign(&this->as.front());
    sch1.assign((TPC*)&this->cs.front());

    EXPECT_EQ(this->as[3], srs1[0][1]);
    EXPECT_EQ(this->as[5], srs1(2,1));
    EXPECT_EQ(this->as[7], srs1(2)[1]);

    EXPECT_EQ(TPC(this->cs[6],this->cs[7]), sch1[0][1]);
    EXPECT_EQ(TPC(this->cs[10],this->cs[11]), sch1(2,1));
    EXPECT_EQ(TPC(this->cs[14],this->cs[15]), sch1(2)[1]);


// Array<TR,std::complex<T>> derived features -  continued
    rv << rv1.normalize();
    EXPECT_EQ(rv1[6], rv(6));

    TP r2 = 0.;
    for (int i = 0; i < 10; ++i) {
        r2 += this->a1[i] * this->a1[i];
    }

    EXPECT_NEAR(1., r2, sp<TP>());
    EXPECT_NEAR(1., rv.norm(), s<TP>());

    EXPECT_EQ(9, rv.indofmax());
    EXPECT_EQ(0, rv.indofmin());

    r1 = rv[9];
    EXPECT_EQ(r1, rv.norminf ());

    rv1.sum(rv1, rv);
    EXPECT_EQ(r1 + r1, rv1[9]);
    rv1.diff (rv1, rv);
    EXPECT_EQ(r1, rv1[9]);

    rv1 += rv;
    EXPECT_EQ(r1 + r1, rv1[9]);
    rv1 -= rv;
    EXPECT_EQ(r1, rv1[9]);
    rv1 += rv1;
    EXPECT_EQ(r1 + r1, rv1[9]);

    cv << cv1;
    EXPECT_EQ(cv1[6], cv(6));

    cr1 = cv1[9];
    cv1 += cv;
    EXPECT_EQ(cr1 + cr1, cv1[9]);
    cv1 -= cv;
    EXPECT_EQ(cr1, cv1[9]);
    cv1 += cv1;
    EXPECT_EQ(cr1 + cr1, cv1[9]);

    rm << rm1;
    EXPECT_EQ(rm1[1][0], rm(1,0));

    r1 = rm(1,1);
    rm += rm1;
    EXPECT_EQ(r1 + r1, rm(1,1));
    rm -= rm1;
    EXPECT_EQ(r1, rm(1,1));
    rm += rm;
    EXPECT_EQ(r1 + r1, rm(1,1));

    cm << cm1;
    EXPECT_EQ(cm1[1][0], cm(1,0));

    cr1 = cm(1,1);
    cm += cm1;
    EXPECT_EQ(cr1 + cr1, cm(1,1));
    cm -= cm1;
    EXPECT_EQ(cr1, cm(1,1));
    cm += cm;
    EXPECT_EQ(cr1 + cr1, cm(1,1));

    srm << srm1;
    r1 = srm(1,1);
    srm += srm1;
    EXPECT_EQ(r1 + r1, srm(1,1));
    srm -= srm1;
    EXPECT_EQ(r1, srm(1,1));
    srm += srm;
    EXPECT_EQ(r1 + r1, srm(1,1));

    scm << scm1;
    EXPECT_EQ(scm1[1][0], scm(1,0));

    cr1 = scm(1,1);
    scm += scm1;
    EXPECT_EQ(cr1 + cr1, scm(1,1));
    scm -= scm1;
    EXPECT_EQ(cr1, scm(1,1));
    scm += scm;
    EXPECT_EQ(cr1 + cr1, scm(1,1));


    srbm1.set(1.14);
    srbm << srbm1;
    srbm.set( -.684);
    r1 = srbm(1,0);
    r2 = srbm1(1,0);
    srbm += srbm1;
    EXPECT_EQ(r1 + r2, srbm(1,0));
    srbm -= srbm1;
    EXPECT_EQ(r1, srbm(1,0));

    TPC cr2 = TPC(1.03, -0.79);
    scbm1.set(cr2);
    scbm << scbm1;
    scbm.set(cr1);
    cr1 = scbm(1,0);
    cr2 = scbm1(1,0);
    scbm += scbm1;

    EXPECT_EQ(cr1 + cr2, scbm(1,0));
    scbm -= scbm1;
    EXPECT_EQ(cr1, scbm(1,0));


    basic_srsmatrix<TP> srs2;
    basic_schmatrix<TP,TPC> sch2;
    srs2 << srs1;
    sch2 << sch1;
    EXPECT_EQ(0. , (srs2 - srs1).norm());
    EXPECT_EQ(0. , (sch2 - sch1).norm());

    basic_srsmatrix<TP> srs2sub {srs2, 1, 2};
    EXPECT_EQ(srs2(1,2), srs2sub(0,1));
    basic_schmatrix<TP,TPC> sch2sub {sch2, 1, 2};
    EXPECT_EQ(sch2(1,2), sch2sub(0,1));


    r2 = 1.13;
    cr2 = TPC(1.03, -0.79);

    TP rs1 = srs2(1,2);
    TPC cs1 = sch2(1,2);
    srs2 *= r2;
    sch2 *= r2;
    EXPECT_EQ(rs1 * r2 , srs2(1,2));
    EXPECT_EQ(cs1 * r2 , sch2(1,2));
    srs2 /= r2;
    sch2 /= r2;
    EXPECT_EQ(rs1, srs2(1,2));
    EXPECT_NEAR(std::abs(cs1), std::abs(sch2(1,2)), sf<TP>());
    EXPECT_NEAR(std::abs(cs1 * cr2), std::abs((sch2 * cr2)(1,2)), sf<TP>());

    basic_rvector<TP> vrs1(3);
    vrs1.randomize(3., 7.);
    EXPECT_NEAR(0., (srs1 * vrs1 - basic_srmatrix<TP>{srs1} * vrs1).norm(), sp<TP>());

    basic_cvector<TP,TPC> vch1(3);
    vch1.randomize_real(3., 7.);
    vch1.randomize_imag(-3., 7.);
    EXPECT_NEAR(0., (sch1 * vch1 - basic_scmatrix<TP,TPC>{sch1} * vch1).norm(), sf<TP>());

    r1 = rv1(8);
    rv1 *= r2;
    EXPECT_EQ(r1 * r2, rv1(8));
    r1 = rm3(6,0);
    rm3 *= r2;
    EXPECT_EQ(r1 * r2, rm3(6,0));
    r1 = srm4(0,1);
    srm4 *= r2;
    EXPECT_EQ(r1 * r2, srm4(0,1));
    r1 = srbm1(0,1);
    srbm1 *= r2;
    EXPECT_EQ(r1 * r2, srbm1(0,1));
    cr1 = scbm1(0,1);
    scbm1 *= cr2;
    EXPECT_NEAR(std::abs(cr1 * cr2), std::abs(scbm1(0,1)), s<TP>());
    r1 = rv1(8);
    rv1 /= r2;
    EXPECT_EQ(r1 / r2, rv1(8));
    r1 = rm3(6,0);

    rm3 /= r2;
    EXPECT_EQ(r1 / r2, rm3(6,0));
    r1 = srm4(0,1);
    srm4 /= r2;
    EXPECT_EQ(r1 / r2, srm4(0,1));
    r1 = srbm1(0,1);
    srbm1 /= r2;
    EXPECT_EQ(r1 / r2, srbm1(0,1));
    cr1 = scbm1(1,0);
    scbm1 /= cr2;
    EXPECT_NEAR(std::abs(cr1 / cr2), std::abs(scbm1(1,0)), sp<TP>());


    cr1 = cv1(8);
    cv1 *= r2;
    EXPECT_EQ(cr1 * r2, cv1(8));
    cr1 = cm1(1,1);
    cm1 *= r2;
    EXPECT_EQ(cr1 * r2, cm1(1,1));
    cr1 = scm1(1,1);
    scm1 *= r2;
    EXPECT_EQ(cr1 * r2, scm1(1,1));
    cr1 = scbm1(0,1);
    scbm1 *= r2;
    EXPECT_EQ(cr1 * r2, scbm1(0,1));
    cr1 = cv1(8);
    cv1 /= r2;
    EXPECT_NEAR(std::abs(cr1 / r2), std::abs(cv1(8)), sf<TP>());
    cr1 = cm1(1,1);
    cm1 /= r2;
    EXPECT_NEAR(std::abs(cr1 / r2), std::abs(cm1(1,1)), sf<TP>());
    cr1 = scm1(1,1);
    scm1 /= r2;
    EXPECT_NEAR(std::abs(cr1 / r2), std::abs(scm1(1,1)), sf<TP>());
    cr1 = scbm1(1,0);
    scbm1 /= r2;
    EXPECT_NEAR(std::abs(cr1 / r2), std::abs(scbm1(1,0)), sf<TP>());


    cr2 = TPC(1.03, -0.79);

    cr1 = cv1(8);
    cv1 *= cr2;
    EXPECT_NEAR(std::abs(cr1 * cr2), std::abs(cv1(8)), sp<TP>());
    cr1 = cm1(1,1);
    cm1 *= cr2;
    EXPECT_NEAR(std::abs(cr1 * cr2), std::abs(cm1(1,1)), sp<TP>());
    cr1 = scm1(1,1);
    scm1 *= cr2;
    EXPECT_NEAR(std::abs(cr1 * cr2), std::abs(scm1(1,1)), sp<TP>());
    cr1 = scbm1(1,0);
    scbm1 *= cr2;
    EXPECT_NEAR(std::abs(cr1 * cr2), std::abs(scbm1(1,0)), spp<TP>());
    cr1 = cv1(8);
    cv1 /= cr2;
    EXPECT_NEAR(std::abs(cr1 / cr2), std::abs(cv1(8)), s<TP>());
    cr1 = cm1(1,1);
    cm1 /= cr2;
    EXPECT_NEAR(std::abs(cr1 / cr2), std::abs(cm1(1,1)), s<TP>());
    cr1 = scm1(1,1);
    scm1 /= cr2;
    EXPECT_NEAR(std::abs(cr1 / cr2), std::abs(scm1(1,1)), sp<TP>());
    cr1 = scbm1(0,1);
    scbm1 /= cr2;
    EXPECT_NEAR(std::abs(cr1 / cr2), std::abs(scbm1(1,1)), sp<TP>());

    srbm << srbm1;
    EXPECT_FLOAT_EQ(srbm1(1,2), srbm(1,2));
    EXPECT_FLOAT_EQ(srbm1(0,3), srbm(0,3));
    scbm << scbm1;
    EXPECT_NEAR(std::abs(scbm1(1,2)), std::abs(scbm(1,2)), s<TP>());
    EXPECT_NEAR(std::abs(scbm1(0,3)), std::abs(scbm(0,3)), s<TP>());

    srs2.set(2.3);
    EXPECT_EQ(TP(2.3), srs2(0,2));
    EXPECT_EQ(TP(2.3), srs2(2,1));

    sch2.set_real(2.3);
    EXPECT_EQ(TP(2.3), sch2.real()(0,2));
    EXPECT_EQ(TP(2.3), sch2.real()(2,1));

    r1 = -0.127;
    rv.set(r1);
    EXPECT_EQ(r1, rv[0]);
    rm.set(r1);
    EXPECT_EQ(r1, rm[0][1]);
    srm.set(r1);
    EXPECT_EQ(r1, srm(1,1));
    srbm.set(r1);
    EXPECT_EQ(r1, srbm(1,2));
    EXPECT_EQ(0, srbm(0,3));

    cr2 = TPC(1.3, -0.9);
    cv.set(cr1);
    EXPECT_EQ(cr1, cv[6]);
    cm.set(cr1);
    EXPECT_EQ(cr1, cm[1][2]);
    scm.set(cr1);
    EXPECT_EQ(cr1, scm(2,1));
    scbm.set(cr1);
    EXPECT_EQ(cr1, scbm(2,1));

    rv.assign(1, this->a2);
    EXPECT_EQ(this->a2[1], rv[2]);
    rv.assign(this->a2);
    EXPECT_EQ(this->a2[2], rv[2]);
    rm.assign(this->a2);
    EXPECT_EQ(this->a2[4], rm(0,2));
    srm.assign(this->a2);
    EXPECT_EQ(this->a2[8], srm(2,2));
    srbm.assign(this->a2);
    EXPECT_EQ(this->a2[2], srbm(0,0));
    EXPECT_EQ(this->a2[9], srbm(1,2));

    cv.assign(1, this->c1);
    EXPECT_EQ(this->c1[1], cv[2]);
    cv.assign(this->c1);
    EXPECT_EQ(this->c1[2], cv[2]);
    cm.assign(this->c1);
    EXPECT_EQ(this->c1[4], cm(0,2));
    scm.assign(this->c1);
    EXPECT_EQ(this->c1[8], scm(2,2));
    scbm.assign(this->c1);
    EXPECT_EQ(this->c1[2], scbm(0,0));
    EXPECT_EQ(this->c1[9], scbm(1,2));


    srs2 = srs1;
    EXPECT_TRUE(srs1 == srs2) << "srsmatrix ==";
    srs2.set(1, 2, srs2(1,2) + TP(0.000001));
    EXPECT_FALSE(srs1 == srs2) << "srsmatrix ==";
    EXPECT_TRUE(srs1 != srs2) << "srsmatrix !=";

    sch2 = sch1;
    EXPECT_TRUE(sch1 == sch2) << "schmatrix ==";
    sch2.set(1, 2, sch2(1,2) + TPC(0.000001,0.00001));
    EXPECT_FALSE(sch1 == sch2) << "schmatrix ==";
    EXPECT_TRUE(sch1 != sch2) << "schmatrix !=";

    rv1 = rv;
    EXPECT_EQ(rv(2), rv1[2]) << "rvector = rvector";
    EXPECT_TRUE(rv1 == rv) << "rvector ==";
    EXPECT_FALSE(rv1 != rv) << "rvector !=";
    rm1 = rm;
    EXPECT_EQ(rm(1,2), rm1[1][2]) << "rmatrix = rmatrix";
    EXPECT_TRUE(rm1 == rm) << "rmatrix ==";
    EXPECT_FALSE(rm1 != rm) << "rmatrix !=";
    EXPECT_TRUE(rm1[0] == rm[0]) << "rmatrix = rmatrix, rm1[1] == rm[1]";
    EXPECT_FALSE(rm1(1) != rm(1)) << "rmatrix = rmatrix,rm1(2) != rm(2)";
    srm1 = srm;
    EXPECT_EQ(srm(1,2), srm1[1][2]) << "srmatrix = srmatrix";
    EXPECT_TRUE(srm1 == srm) << "srmatrix ==";
    EXPECT_FALSE(srm1 != srm) << "srmatrix !=";
    EXPECT_TRUE(srm1[0] == srm[0]) << "srmatrix = srmatrix, srm1[1] == srm[1]";
    EXPECT_FALSE(srm1(1) != srm(1)) << "srmatrix = srmatrix,srm1(2) != srm(2)";
    srbm1 = srbm;
    EXPECT_EQ(srbm(1,0), srbm1[1][0]) << "srbmatrix = srbmatrix";
    EXPECT_TRUE(srbm1 == srbm) << "srbmatrix ==";
    EXPECT_FALSE(srbm1 != srbm) << "srbmatrix !=";
    EXPECT_TRUE(srbm1[0] == srbm[0]) << "srbmatrix = srbmatrix, srbm1[1] == srbm[1]";
    EXPECT_FALSE(srbm1(1) != srbm(1)) << "srbmatrix = srbmatrix,srbm1(2) != srbm(2)";

    cv1 = cv;
    EXPECT_EQ(cv(3), cv1[3]) << "cvector = cvector";
    EXPECT_TRUE(cv1 == cv) << "cvector ==";
    EXPECT_FALSE(cv1 != cv) << "cvector !=";
    cm1 = cm;
    EXPECT_EQ(cm(1,0), cm1[1][0]) << "cmatrix = cmatrix";
    EXPECT_TRUE(cm1 == cm) << "cmatrix ==";
    EXPECT_FALSE(cm1 != cm) << "cmatrix !=";
    EXPECT_TRUE(cm1[0] == cm[0]) << "cmatrix = cmatrix, cm1[1] == cm[1]";
    EXPECT_FALSE(cm1(1) != cm(1)) << "cmatrix = cmatrix,cm1(2) != cm(2)";
    scm1 = scm;
    EXPECT_EQ(scm(1,0), scm1[1][0]) << "scmatrix = scmatrix";
    EXPECT_TRUE(scm1 == scm) << "scmatrix ==";
    EXPECT_FALSE(scm1 != scm) << "scmatrix !=";
    EXPECT_TRUE(scm1[0] == scm[0]) << "scmatrix = scmatrix, scm1[1] == scm[1]";
    EXPECT_FALSE(scm1(1) != scm(1)) << "scmatrix = scmatrix,scm1(2) != scm(2)";
    scbm1 = scbm;
    EXPECT_EQ(scbm(1,0), scbm1[1][0]) << "scbmatrix = scbmatrix";
    EXPECT_TRUE(scbm1 == scbm) << "scbmatrix ==";
    EXPECT_FALSE(scbm1 != scbm) << "scbmatrix !=";
    EXPECT_TRUE(scbm1[0] == scbm[0]) << "scbmatrix = scbmatrix, scbm1[1] == scbm[1]";
    EXPECT_FALSE(scbm1(1) != scbm(1)) << "scbmatrix = scbmatrix,scbm1(2) != scbm(2)";

//        rv2 = rv + rv1;   // wouldn't work because rv and rv1 share the same array!
    rv3.resize(10);
    rv3 = rv + rv1;
    EXPECT_EQ(rv(0) + rv1[0], rv3[0]) << "rvector + rvector";
    EXPECT_EQ(rv(9) + rv1[9], rv3[9]) << "rvector + rvector";
    rv3 = rv - rv1;
    EXPECT_EQ(rv(0) - rv1[0], rv3[0]) << "rvector - rvector";
    EXPECT_EQ(rv(9) - rv1[9], rv3[9]) << "rvector - rvector";
    cv3 = cv + cv1;
    EXPECT_EQ(cv(0) + cv1[0], cv3[0]) << "cvector + cvector";
    EXPECT_EQ(cv(9) + cv1[9], cv3[9]) << "cvector + cvector";
    cv3 = cv - cv1;
    EXPECT_EQ(cv(0) - cv1[0], cv3[0]) << "cvector - cvector";
    EXPECT_EQ(cv(9) - cv1[9], cv3[9]) << "cvector - cvector";
    rm2.resize(2, 3);
    rm = rm1 + rm2;
    EXPECT_EQ(rm1(0,0) + rm2(0, 0), rm[0][0]) << "rmatrix + rmatrix";
    EXPECT_EQ((rm1[1] + rm2[1]).norm(),rm[1].norm()) << "rmatrix + rmatrix";
    rm = rm1 - rm2;
    EXPECT_EQ(rm1(0,0) - rm2(0, 0), rm[0][0]) << "rmatrix - rmatrix";
    EXPECT_EQ((rm1(2) - rm2(2)).norm(),rm(2).norm()) << "rmatrix - rmatrix";
    cm2.resize(2, 3);
    cm = cm1 + cm2;
    EXPECT_EQ(cm1(0,0) + cm2(0, 0), cm[0][0]) << "cmatrix + cmatrix";
    EXPECT_EQ((cm1[1] + cm2[1]).norm(),cm[1].norm()) << "cmatrix + cmatrix";
    cm = cm1 - cm2;
    EXPECT_EQ(cm1(0,0) - cm2(0, 0), cm[0][0]) << "cmatrix - cmatrix";
    EXPECT_EQ((cm1(2) - cm2(2)).norm(),cm(2).norm()) << "cmatrix - cmatrix";
    srm = srm1 + srm2;
    EXPECT_EQ(srm1(0,0) + srm2(0, 0), srm[0][0]) << "srmatrix + srmatrix";
    EXPECT_NEAR((srm1[2] + srm2[2]).norm(),srm[2].norm(), s<TP>()) << "srmatrix + srmatrix";
    srm = srm1 - srm2;
    EXPECT_EQ(srm1(0,0) - srm2(0, 0), srm[0][0]) << "srmatrix - srmatrix";
    EXPECT_EQ((srm1(2) - srm2(2)).norm(),srm(2).norm()) << "srmatrix - srmatrix";
    scm = scm1 + scm2;
    EXPECT_EQ(scm1(0,0) + scm2(0, 0), scm[0][0]) << "scmatrix + scmatrix";
    EXPECT_EQ((scm1(2) + scm2(2)).norm(),scm(2).norm()) << "scmatrix + scmatrix";
    scm = scm1 - scm2;
    EXPECT_EQ(scm1(0,0) - scm2(0, 0), scm[0][0]) << "scmatrix - scmatrix";
    EXPECT_EQ((scm1(2) - scm2(2)).norm(),scm(2).norm()) << "scmatrix - scmatrix";

    basic_srbmatrix<TP> srbm2 {this->a2, 4, 1, 2};
    srbm = srbm1 + srbm2;
    EXPECT_EQ(srbm1(0,0) + srbm2(0, 0), srbm[0][0]) << "srbmatrix + srbmatrix";
    EXPECT_EQ((srbm1(2) + srbm2(2)).norm(),srbm(2).norm()) << "srbmatrix + srbmatrix";
    srbm = srbm1 - srbm2;
    EXPECT_EQ(srbm1(0,0) - srbm2(0, 0), srbm[0][0]) << "srbmatrix - srbmatrix";
    EXPECT_EQ((srbm1(2) - srbm2(2)).norm(),srbm(2).norm()) << "srbmatrix - srbmatrix";

    basic_scbmatrix<TP,TPC> scbm2 {this->c1, 4, 1, 2};
    scbm = scbm1 + scbm2;
    EXPECT_EQ(scbm1(0,0) + scbm2(0, 0), scbm[0][0]) << "scbmatrix + scbmatrix";
    EXPECT_EQ((scbm1(2) + scbm2(2)).norm(),scbm(2).norm()) << "scbmatrix + scbmatrix";
    scbm = scbm1 - scbm2;
    EXPECT_EQ(scbm1(0,0) - scbm2(0, 0), scbm[0][0]) << "scbmatrix - scbmatrix";
    EXPECT_EQ((scbm1(2) - scbm2(2)).norm(),scbm(2).norm()) << "scbmatrix - scbmatrix";

    srs2 = srs1;
    rs1 = srs1(0, 1);
    EXPECT_EQ(rs1 + rs1,(srs1 + srs2)(0,1)) << "srsmatrix + srsmatrix";
    EXPECT_EQ(0.,(srs1 - srs2).norm()) << "srsmatrix - srsmatrix";

    sch2 = sch1;
    cs1 = sch1(0, 1);
    EXPECT_EQ(cs1 + cs1,(sch1 + sch2)(0,1)) << "schmatrix + schmatrix";
    EXPECT_EQ(0.,(sch1 - sch2).norm()) << "schmatrix - schmatrix";


    int n1 = -2;
    r1     = -2.;
    rv1 = rv * r1;
    rv3 = n1 * rv;
    EXPECT_EQ(rv1[2], rv3[2]) << "rvector * number";
    rv3 = r1 * rv;
    EXPECT_EQ(rv1[2], rv3[2]) << "rvector * number";
    cv1 = cv * r1;
    cv3 = n1 * cv;
    EXPECT_EQ(cv1[2], cv3[2]) << "cvector * number";
    cv3 = r1 * cv;
    EXPECT_EQ(cv1[2], cv3[2]) << "cvector * number";
    rm1 = rm * r1;
    rm2 = n1 * rm;
    EXPECT_EQ(rm2(1,2), rm1(1,2)) << "rmatrix * number";
    rm2 = r1 * rm;
    EXPECT_EQ(rm2(1,2), rm1(1,2)) << "rmatrix * number";
    cm1 = cm * r1;
    cm2 = n1 * cm;
    EXPECT_EQ(cm1(1,2), cm2(1,2)) << "cmatrix * number";
    cm2 = r1 * cm;
    EXPECT_EQ(cm1(1,2), cm2(1,2)) << "cmatrix * number";
    srm1 = srm * r1;
    srm2 = n1 * srm;
    EXPECT_EQ(srm2(1,2), srm1(1,2)) << "srmatrix * number";
    srm2 = r1 * srm;
    EXPECT_EQ(srm2(1,2), srm1(1,2)) << "srmatrix * number";
    scm.assign(this->c1);
    scm1 = scm * r1;
    scm2 = n1 * scm;
    EXPECT_EQ(scm1(1,2), scm2(1,2)) << "scmatrix * number";
    scm2 = r1 * scm;
    EXPECT_EQ(scm1(1,2), scm2(1,2)) << "scmatrix * number";
    srbm1 = srbm * r1;
    srbm2 = n1 * srbm;
    EXPECT_EQ(srbm2(1,2), srbm1(1,2)) << "srbmatrix * number";
    srbm2 = r1 * srbm;
    EXPECT_EQ(srbm2(1,2), srbm1(1,2)) << "srbmatrix * number";
    scbm1 = scbm * r1;
    scbm2 = n1 * scbm;
    EXPECT_EQ(scbm2(1,2), scbm1(1,2)) << "scbmatrix * number";
    scbm2 = r1 * scbm;
    EXPECT_EQ(scbm2(1,2), scbm1(1,2)) << "scbmatrix * number";
    cr1 = r1;
    scbm2 = cr1 * scbm;
    EXPECT_EQ(scbm2(1,2), scbm1(1,2)) << "scbmatrix * number";

    rv1 = rv / r1;
    EXPECT_EQ(rv[9] / r1, rv1[9]) << "rvector / number";
    cv1 = cv / r1;
    EXPECT_EQ(cv[9] / r1, cv1[9]) << "cvector / number";
    rm1 = rm / r1;
    EXPECT_EQ(rm(1,2) / r1, rm1(1,2)) << "rmatrix / number";
    cm1 = cm / r1;
    EXPECT_EQ(cm(1,2) / r1, cm1(1,2)) << "cmatrix / number";

    srm1 = srm / r1;
    EXPECT_EQ(srm(1,2) / r1, srm1(1,2)) << "srmatrix / number";
    scm1 = scm / r1;
    EXPECT_EQ(scm(1,2) / r1, scm1(1,2)) << "scmatrix / number";
    srbm1 = srbm / r1;
    EXPECT_EQ(srbm(1,2) / r1, srbm1(1,2)) << "srbmatrix / number";
    scbm1 = scbm / r1;
    EXPECT_EQ(scbm(1,2) / r1, scbm1(1,2)) << "scbmatrix / number";
    scbm1 = scbm / cr1;
    EXPECT_EQ(scbm(1,2) / cr1, scbm1(1,2)) << "scbmatrix / number";

    cv1 = cv * cr2;
    cv3 = cr2 * cv;
    EXPECT_EQ(cv1[2], cv3[2]) << "cvector * cmplx number";
    cm1 = cm * cr2;
    cm2 = cr2 * cm;
    EXPECT_EQ(cm1(1,2), cm2(1,2)) << "cmatrix * cmplx number";
    scm1 = scm * cr2;
    scm2 = cr2 * scm;
    EXPECT_EQ(scm1(1,2), scm2(1,2)) << "scmatrix * cmplx number";
    scbm1 = scbm * cr2;
    scbm2 = cr2 * scbm;
    EXPECT_EQ(scbm1(1,2), scbm2(1,2)) << "scbmatrix * cmplx number";

    rv1 = - rv;
    EXPECT_EQ(- rv[9], rv1[9]) << "- rvector";
    cv1 = - cv;
    EXPECT_EQ(- cv[9], cv1[9]) << "- cvector";
    rm1 = - rm;
    EXPECT_EQ(- rm(1,2), rm1(1,2)) << "- rmatrix";
    cm1 = - cm;
    EXPECT_EQ(- cm(1,2), cm1(1,2)) << "- cmatrix";
    srm1 = - srm;
    EXPECT_EQ(- srm(1,2), srm1(1,2)) << "- srmatrix";
    scm1 = - scm;
    EXPECT_EQ(- scm(1,2), scm1(1,2)) << "- scmatrix";
    srbm.assign(this->a2);
    srbm1 = - srbm;
    EXPECT_EQ(- srbm(1,2), srbm1(1,2)) << "- srbmatrix";
    scbm.assign(this->c1);
    scbm1 = - scbm;
    EXPECT_EQ(- scbm(1,2), scbm1(1,2)) << "- scbmatrix";


    rv1.set(1.17);
    rv2.set(-0.31);
    rm2.set(9.01);
    srbm1.set(13.1);
    srbm2.set(5.51);
    cv1.set(TPC(2,1));
    cv2.set(TPC(-1,3));
    cm2.set(TPC(-4,3));
    rv1.resize(2);
    rv2.resize(3);
    rv2.mult(rv1, rm2);

    EXPECT_NEAR(rv1 * rm2(0), rv2[0], sp<TP>()) << "mult";
    rv1.mult(rm2, rv2);
    EXPECT_NEAR(rv2 * rm2[0], rv1[0], sp<TP>()) << "mult";

    cv1.resize(2);
    cv2.resize(3);
    cv2.mult(cv1, cm2);

    EXPECT_EQ(cv1 * cm2(0), cv2[0]) << "mult";
    cv1.mult(cm2, cv2);
    EXPECT_EQ(cv2 * cm2[0], cv1[0]) << "mult";

    rv1.resize(3);
    rv1.mult(srm2, rv2);
    EXPECT_FLOAT_EQ(rv2 * srm2[0], rv1[0]) << "mult";
    rv2.mult(rv1, srm2);

    EXPECT_FLOAT_EQ(static_cast<float>(rv1 * srm2(0)), static_cast<float>(rv2[0])) << "mult";

    cv1.resize(3);
    cv1.mult(scm2, cv2);
    EXPECT_FLOAT_EQ(static_cast<float>((cv2 * scm2[0]).real()), static_cast<float>(cv1[0].real())) << "mult";
    EXPECT_FLOAT_EQ(static_cast<float>((cv2 * scm2[0]).imag()), static_cast<float>(cv1[0].imag())) << "mult";
    cv2.mult(cv1, scm2);

    EXPECT_NEAR(std::abs(cv1 * scm2(0)), std::abs(cv2[0]), sp<TP>()) << "mult";

//    EXPECT_FLOAT_EQ(static_cast<float>((cv1 * scm2(0)).real()), static_cast<float>(cv2[0].real())) << "mult";
//    EXPECT_FLOAT_EQ(static_cast<float>((cv1 * scm2(0)).imag()), static_cast<float>(cv2[0].imag())) << "mult";

    rv1.resize(4);
    rv2.resize(4);
    rv2.mult(rv1, srbm2);
    EXPECT_EQ(rv1 * srbm2(0), rv2[0]) << "mult";
    rv1.mult(srbm2, rv2);
    EXPECT_EQ(rv2 * srbm2[0], rv1[0]) << "mult";

    cv1.resize(4);
    cv2.resize(4);
    cv2.mult(cv1, scbm2);
    EXPECT_EQ(cv1 * scbm2(0), cv2[0]) << "mult";
    cv1.mult(scbm2, cv2);
    EXPECT_EQ(cv2 * scbm2[0], cv1[0]) << "mult";

    rm1.resize(3, 2);
    rm1[2].assign(this->a1);
    rm3.resize(2, 2);
    rm4.resize(3, 3);
    rm3.mult(rm2, rm1);
    EXPECT_NEAR(rm2[1] * rm1(1),rm3(1,1), sp<TP>()) << "mult";
    rm4.mult(rm1, rm2);
    EXPECT_NEAR(rm1[2] * rm2(2),rm4(2,2), sp<TP>()) << "mult";
    srm4.resize(3);
    srm4.mult(rm1, rm2);

    EXPECT_NEAR(rm1[2] * rm2(2),srm4(2,2), sp<TP>()) << "mult";
    rm4.resize(3, 2);
    rm1.mult(srm4, rm4);
    EXPECT_NEAR(srm4[2] * rm4(1),rm1(2,1), sp<TP>()) << "mult";
    srbm1.resize(3);
    rm1.mult(srbm1, rm4);
    EXPECT_NEAR(srbm1[2] * rm4(1),rm1(2,1), sp<TP>()) << "mult";
    rm1.mult(~srbm1, rm4);
    tmp = static_cast<float>(rm1(2,1));
    EXPECT_FLOAT_EQ(static_cast<float>(srbm1(2) * rm4(1)),tmp) << "mult";
    srbm1.mult(rm1, rm2);
    EXPECT_NEAR(rm1[1] * rm2(1),srbm1(1,1), sp<TP>()) << "mult";

    r1 = -0.031;
    r2 = 0.319;
    rm1.randomize(1., 2.);
    rm2.randomize(0., 1.);
    rm3.randomize(0., 1.);
    basic_rmatrix<TP> rm3_dub = rm3;

    rm3.gemm(rm2, false, rm1, false, r1, r2);
    EXPECT_NEAR(0.,(rm3 - (rm2 * rm1 * r1 + rm3_dub * r2)).norm2(), s<TP>()) << "gemm";
    rm3_dub = rm3;
    rm3 << ~rm3;
    rm3.gemm(rm1, true, rm2, true, r1, r2);
    EXPECT_NEAR(0.,(~rm3 - (rm2 * rm1 * r1 + rm3_dub * r2)).norm2(), s<TP>()) << "gemm";

    srbm1.randomize(-1., 3.);
    basic_rmatrix<TP> rm1_dub = rm1;
    rm1.gemm(srbm1, false, rm4, false, r1, r2);
    EXPECT_NEAR(0.,(rm1 - (srbm1 * rm4 * r1 + rm1_dub * r2)).norm2(), sp<TP>()) << "gemm";


    cm1.resize(3, 2);
    cm1[2].assign(this->c1);
    basic_cmatrix<TP,TPC> cm3 {2, 2}, cm4 {3, 3};
    cm3.assign(this->c2);
    cm3.mult(cm2, cm1);
    EXPECT_FLOAT_EQ(static_cast<float>((cm2[1] * cm1(1)).real()),
        static_cast<float>(cm3(1,1).real())) << "mult";
    EXPECT_FLOAT_EQ(static_cast<float>((cm2[1] * cm1(1)).imag()),
        static_cast<float>(cm3(1,1).imag())) << "mult";
    cm4.mult(cm1, cm2);
    EXPECT_FLOAT_EQ((cm1[2] * cm2(2)).real(),cm4(2,2).real()) << "mult";
    EXPECT_FLOAT_EQ((cm1[2] * cm2(2)).imag(),cm4(2,2).imag()) << "mult";
    scm4.resize(3);
    scm4.mult(cm1, cm2);
    EXPECT_FLOAT_EQ((cm1[2] * cm2(2)).real(),scm4(2,2).real()) << "mult";
    EXPECT_FLOAT_EQ((cm1[2] * cm2(2)).imag(),scm4(2,2).imag()) << "mult";
    cm4.resize(3, 2);
    cm1.mult(scm4, cm4);
    EXPECT_FLOAT_EQ((scm4[2] * cm4(1)).real(),cm1(2,1).real()) << "mult";
    EXPECT_FLOAT_EQ((scm4[2] * cm4(1)).imag(),cm1(2,1).imag()) << "mult";
    scbm.resize(3);
    scbm.set(TPC(1.23,-0.912));
    cm1.mult(scbm, cm4);
    EXPECT_FLOAT_EQ((scbm[2] * cm4(1)).real(),cm1(2,1).real()) << "mult";
    EXPECT_FLOAT_EQ((scbm[2] * cm4(1)).imag(),cm1(2,1).imag()) << "mult";
    cm1.mult(~scbm, cm4);
    EXPECT_FLOAT_EQ(static_cast<float>((~(scbm(2)) * cm4(1)).real()), static_cast<float>(cm1(2,1).real())) << "mult";
    EXPECT_FLOAT_EQ(static_cast<float>((~(scbm(2)) * cm4(1)).imag()), static_cast<float>(cm1(2,1).imag())) << "mult";
    scbm1.resize(3);
    scbm1.mult(cm1, cm2);
    EXPECT_FLOAT_EQ((cm1[1] * cm2(1)).real(),scbm1(1,1).real()) << "mult";
    EXPECT_FLOAT_EQ((cm1[1] * cm2(1)).imag(),scbm1(1,1).imag()) << "mult";

    cm1.randomize_real(0., 1.);
    cm2.randomize_real(0., 1.);
    cm3.randomize_real(0., 1.);
    scbm.randomize_real(0., 1.);
    basic_cmatrix<TP,TPC> cm3_dub = cm3;
    cm3.gemm(cm2, false, cm1, false, cr1, cr2);
    EXPECT_NEAR(0.,(cm3 - (cm2 * cm1 * cr1 + cm3_dub * cr2)).norm(), sp<TP>()) << "gemm";
    basic_cmatrix<TP,TPC> cm1_dub = cm1;
    cm1.gemm(scbm, false, cm4, false, cr1, cr2);
    EXPECT_NEAR(0.,(cm1 - (scbm * cm4 * cr1 + cm1_dub * cr2)).norm(), sp<TP>()) << "gemm";

    cr1 = TPC(-1.14,3.22);
    cr2 = TPC(2.04,-4.2);
    cm1_dub << cm1;
    cm1.conj();
    cm1.gemm(cm4, true, scbm, true, cr1, cr2);
    EXPECT_NEAR(0.,(~cm1 - (scbm * cm4 * std::conj(cr1) + cm1_dub * std::conj(cr2))).norm2(), sp<TP>()) << "gemm";
    cm1.conj();

    rv1.randomize(0., 1.);
    rv2.randomize(0., 1.);
    tmp = static_cast<float>(rv1 * rv2);
    EXPECT_FLOAT_EQ(static_cast<float>(rv1[0]*rv2[0]+rv1[1]*rv2[1]+rv1[2]*rv2[2]+rv1[3]*rv2[3]),
        tmp) << "scalar product";
    cv1.randomize_real(0., 1.);

    cv1.randomize_imag(0., 1.);
    cv2.randomize_real(0., 1.);
    cv2.randomize_imag(0., 1.);
    EXPECT_NEAR(static_cast<float>((cv1[0]*cv2[0]+cv1[1]*cv2[1]+
        cv1[2]*cv2[2]+cv1[3]*cv2[3]).real()),
        static_cast<float>((cv1 * cv2).real()), sf<TP>()) << "scalar product";
    EXPECT_NEAR(static_cast<float>((cv1[0]*cv2[0]+cv1[1]*cv2[1]+
        cv1[2]*cv2[2]+cv1[3]*cv2[3]).imag()),
        static_cast<float>((cv1 * cv2).imag()), sf<TP>()) << "scalar product";
    EXPECT_NEAR(static_cast<float>((std::conj(cv1[0])*cv2[0]+std::conj(cv1[1])*cv2[1]+
        std::conj(cv1[2])*cv2[2]+std::conj(cv1[3])*cv2[3]).real()),
        static_cast<float>((cv1 % cv2).real()), sf<TP>()) << "scalar product, conj";
    EXPECT_NEAR(static_cast<float>((std::conj(cv1[0])*cv2[0]+std::conj(cv1[1])*cv2[1]+
        std::conj(cv1[2])*cv2[2]+std::conj(cv1[3])*cv2[3]).imag()),
        static_cast<float>((cv1 % cv2).imag()), sf<TP>()) << "scalar product, conj";

    EXPECT_EQ(0.,(rm1[1] - (~rm1)(1)).norm()) << "~";

    basic_cvector<TP,TPC> cm1_2_conj(cm1[1].size());
    cm1_2_conj = cm1[1];
    cm1_2_conj.conj();
    EXPECT_EQ(0.,(cm1_2_conj - (~cm1)(1)).norm()) << "~";
    EXPECT_EQ(0.,(srbm1[1] - (~srbm1)(1)).norm()) << "~";
    EXPECT_EQ(0.,(~(scbm1[1]) - (~scbm1)(1)).norm()) << "~";

    rv1.resize(3);
    rv2.resize(2);
    rv1 = rm1 * rv2;
    EXPECT_FLOAT_EQ(rv2 * rm1[2], rv1[2]) << "rmatrix * rvector";
    rv2 = rv1 * rm1;
    EXPECT_FLOAT_EQ(static_cast<float>(rv1 * rm1(1)), static_cast<float>(rv2[1])) << "rvector * rmatrix";
    cv1.resize(3);
    cv2.resize(2);
    cv1 = cm1 * cv2;
    EXPECT_FLOAT_EQ(static_cast<float>((cv2 * cm1[2]).real()),
        static_cast<float>(cv1[2].real())) << "cmatrix * cvector";
    EXPECT_NEAR(static_cast<float>((cv2 * cm1[2]).imag()),
        static_cast<float>(cv1[2].imag()), spp<TP>(1.e-7,1.e-4)) << "cmatrix * cvector";
    cv2 = cv1 * cm1;
    EXPECT_FLOAT_EQ(static_cast<float>((cv1 * cm1(1)).real()),
        static_cast<float>(cv2[1].real())) << "cvector * cmatrix";
    EXPECT_NEAR(static_cast<float>((cv1 * cm1(1)).imag()),
        static_cast<float>(cv2[1].imag()), spp<TP>(1.e-7,0.2)) << "cvector * cmatrix";

    rv2.resize(3);
    rv2 = srm4 * rv1;
    EXPECT_FLOAT_EQ(static_cast<float>(rv1 * srm4[2]), static_cast<float>(rv2[2])) << "srmatrix * rvector";
    rv2 = rv1 * srm4;
    EXPECT_FLOAT_EQ(static_cast<float>(rv1 * srm4(2)), static_cast<float>(rv2[2])) << "rvector * srmatrix";
    cv2.resize(3);
    cv2 = scm4 * cv1;
    EXPECT_FLOAT_EQ(static_cast<float>((cv1 * scm4[2]).real()),
        static_cast<float>(cv2[2].real())) << "scmatrix * cvector";
    EXPECT_FLOAT_EQ(static_cast<float>((cv1 * scm4[2]).imag()),
        static_cast<float>(cv2[2].imag())) << "scmatrix * cvector";
    cv2 = cv1 * scm4;
    EXPECT_NEAR(static_cast<float>((cv1 * scm4(2)).real()),
        static_cast<float>(cv2[2].real()), spp<TP>()) << "cvector * scmatrix";
    EXPECT_NEAR(static_cast<float>((cv1 * scm4(2)).imag()),
        static_cast<float>(cv2[2].imag()), spp<TP>()) << "cvector * scmatrix";

    srbm1.normalize();
    rv1.normalize();
    rv2 = srbm1 * rv1;
    EXPECT_NEAR(rv1 * srbm1[2], rv2[2], s<TP>()) << "srbmatrix * rvector";
    rv2 = rv1 * srbm1;
    EXPECT_NEAR(rv1 * srbm1(2), rv2[2], s<TP>()) << "rvector * srbmatrix";

    scbm1.normalize();
    cv1.normalize();
    cv2 = scbm1 * cv1;
    EXPECT_NEAR(static_cast<float>((cv1 * scbm1[2]).real()),
        static_cast<float>(cv2[2].real()), s<TP>()) << "scbmatrix * cvector";
    EXPECT_NEAR(static_cast<float>((cv1 * scbm1[2]).imag()),
        static_cast<float>(cv2[2].imag()), s<TP>()) << "scbmatrix * cvector";
    cv2 = cv1 * scbm1;
    EXPECT_NEAR(static_cast<float>((cv1 * scbm1(2)).real()),
        static_cast<float>(cv2[2].real()), s<TP>()) << "cvector * scbmatrix";
    EXPECT_NEAR(static_cast<float>((cv1 * scbm1(2)).imag()),
        static_cast<float>(cv2[2].imag()), s<TP>()) << "cvector * scbmatrix";

    rv2.resize(2);
    rm1 = rv1.rank1update(rv2);
    EXPECT_EQ(rv1[2] * rv2[0],rm1(2,0)) << "rank1update";
    rm1.rank1update(rv1, rv2);
    EXPECT_EQ(rv1[2] * rv2[1],rm1(2,1)) << "rank1update";

    cv2.resize(2);
    cv1.normalize();
    cv2.normalize();
    cm1 = cv1.rank1update_u(cv2);
    EXPECT_NEAR(std::abs(cv1[2] * cv2[0]), std::abs(cm1(2,0)), s<TP>()) << "rank1update_u";
    cm1.rank1update_u(cv1, cv2);
    EXPECT_NEAR(std::abs(cv1[2] * cv2[1]), std::abs(cm1(2,1)), s<TP>()) << "rank1update_u";
    cm1 = cv1.rank1update_c(cv2);
    EXPECT_NEAR(std::abs(cv1[2] * std::conj(cv2[0])), std::abs(cm1(2,0)), s<TP>()) << "rank1update_c";
    cm1.rank1update_c(cv1, cv2);
    EXPECT_NEAR(std::abs(cv1[2] * std::conj(cv2[1])), std::abs(cm1(2,1)), s<TP>()) << "rank1update_c";

    srm4.assign(this->a3);
    srm4(2, 2) = -1.;
    srm4.normalize();
    EXPECT_NEAR(1. / (srm4.norminf() * srm4.inv().norminf()), srm4.cond(), s<TP>()) << "cond";
    TP dt = srm4.det();
    TP dt_expected = srm4(0,0) * srm4(1,1) * srm4(2, 2) -
        srm4(0,0) * srm4(1,2) * srm4(2, 1) -
        srm4(0,1) * srm4(1,0) * srm4(2, 2) +
        srm4(0,1) * srm4(1,2) * srm4(2, 0) +
        srm4(0,2) * srm4(1,0) * srm4(2, 1) -
        srm4(0,2) * srm4(1,1) * srm4(2, 0);
    EXPECT_NEAR(dt_expected, dt, s<TP>()) << "det";

    scm4.assign(this->c2);
    scm4.normalize();
    EXPECT_NEAR(1. / (scm4.norminf() * scm4.inv().norminf()), scm4.cond(), s<TP>()) << "cond";
    TPC dtc = scm4.det();
    TPC dtc_expected = scm4(0,0) * scm4(1,1) * scm4(2, 2) -
        scm4(0,0) * scm4(1,2) * scm4(2, 1) -
        scm4(0,1) * scm4(1,0) * scm4(2, 2) +
        scm4(0,1) * scm4(1,2) * scm4(2, 0) +
        scm4(0,2) * scm4(1,0) * scm4(2, 1) -
        scm4(0,2) * scm4(1,1) * scm4(2, 0);
    EXPECT_NEAR(std::norm(dtc_expected), std::norm(dtc), s<TP>()) << "complex det";

    r1 = 2.;
    rv1.resize(4);
    rm1.resize(4, 4);
    srbm1.resize(4);
    rv1.set(1.);
    rm1 << basic_eye_real<TP>(4);
    srm4 << basic_eye_real<TP>(4);
    srbm1 << basic_srbmatrix<TP>{basic_eye_real<TP>(4), 0, 0};

    EXPECT_EQ(r1,rv1.norm()) << "rvector norm";
    EXPECT_EQ(r1,rm1.norm()) << "rmatrix norm";
    EXPECT_EQ(r1,srm4.norm()) << "srmatrix norm";
    EXPECT_EQ(r1,srbm1.norm()) << "srbmatrix norm";

    r1 = TP(2. * sqrt(2.));
    cv1.resize(4);
    cm1.resize(4, 4);
    scm1.resize(4);
    cv1.set(TPC(1,1));
    cm1 << basic_scmatrix<TP,TPC>{cv1};
    scm1 = cm1;
    scbm2 = basic_scbmatrix<TP,TPC>{cm1,scbm2.lsize(),scbm2.usize()};

    EXPECT_EQ(r1,cv1.norm()) << "cvector norm";
    EXPECT_EQ(r1,cm1.norm()) << "cmatrix norm";
    EXPECT_EQ(r1,scm1.norm()) << "scmatrix norm";
    EXPECT_EQ(r1,scbm2.norm()) << "scbmatrix norm";

    // mix
    scbm2.set(TPC(1.23,-0.977));

    cm1 = scbm2;
    EXPECT_NEAR(std::abs(scbm2(1,2)), std::abs(cm1(1,2)), s<TP>()) << "mix cmatrix scbm";
    EXPECT_NEAR(std::abs(scbm2(3,0)), std::abs(cm1(3,0)), s<TP>()) << "mix cmatrix scbm";

    cm1 = cm1 + scbm2;
    cm1 += scbm2;
    EXPECT_NEAR((scbm2(1,2) * TP(3.)).real(), cm1(1,2).real(), spp<TP>()) << "mix cmatrix scbm";
    EXPECT_NEAR((scbm2(1,2) * TP(3.)).imag(), cm1(1,2).imag(), spp<TP>()) << "mix cmatrix scbm";
    EXPECT_NEAR((scbm2(3,0) * TP(3.)).real(), cm1(3,0).real(), spp<TP>()) << "mix cmatrix scbm";
    EXPECT_NEAR((scbm2(3,0) * TP(3.)).imag(), cm1(3,0).imag(), spp<TP>()) << "mix cmatrix scbm";
    EXPECT_NEAR((TP(3.) * scbm2(1,2)).real(), cm1(0,1).real(), spp<TP>()) << "mix cmatrix scbm";
    EXPECT_NEAR((TP(3.) * scbm2(1,2)).imag(), cm1(0,1).imag(), spp<TP>()) << "mix cmatrix scbm";
    EXPECT_NEAR((TP(3.) * scbm2(1,0)).real(), cm1(1,0).real(), spp<TP>()) << "mix cmatrix scbm";
    EXPECT_NEAR((TP(3.) * scbm2(1,0)).imag(), cm1(1,0).imag(), spp<TP>()) << "mix cmatrix scbm";

    rm1 = srbm2;
    EXPECT_NEAR(srbm2(1,2), rm1(1,2), s<TP>()) << "mix rmatrix srbm";
    EXPECT_NEAR(srbm2(3,0), rm1(3,0), s<TP>()) << "mix rmatrix srbm";

    rm1 = rm1 + srbm2;
    rm1 += srbm2;
    EXPECT_EQ(srbm2(1,2) * 3., rm1(1,2)) << "mix rmatrix srbm";
    EXPECT_EQ(3. * srbm2(3,0), rm1(3,0)) << "mix matrix  srbm";
    EXPECT_EQ(3 * srbm2(3,0), rm1(3,0)) << "mix rmatrix srbm";
    EXPECT_EQ(srbm2(1,2) * 3, rm1(1,2)) << "mix rmatrix srbm";

    scbm1.resize(4);
    for (int j = 0; j <= 3; j++) {
        for (int i = 0; i <= 3; i++) {
            rm1(i,j)  = - TP(j * 4 + i + 1);
            srm4(i,j) = - TP(j * 4 + i + 1);
            cm1(i,j)  = - TP(j * 4 + i + 1);
            scm1(i,j) = - TP(j * 4 + i + 1);
        }
        srbm1(j,j) = TP(j + 1);
        scbm1(j,j) = TPC(TP(j + 1));
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

    EXPECT_EQ(1.,basic_eye_real<TP>(6)(5,5)) << "basic_eye_real<TP>";
    basic_scmatrix<TP,TPC> ec = basic_eye_complex<TP,TPC>(6);
    EXPECT_EQ(TPC(1,0), ec(5,5)) << "basic_eye_complex<TP,TPC>";

    rv2.resize(4);
    basic_srmatrix<TP> rmU{4}, rmVH{4};
    rv1 = srm4.svd(rmU, rmVH);
    rv2.svd(srm4, rmU, rmVH);
//        EXPECT_TRUE(rv1 == rv2) << "srmatrix svd";
    EXPECT_EQ(0.,(rv1 - rv2).norm()) << "srmatrix svd";
    srm1 << basic_srmatrix<TP>{rv1};
    EXPECT_NEAR(0., (srm4 * ~rmVH - rmU * srm1).norm(), sp<TP>()) << "srmatrix svd";
    EXPECT_NEAR(0., (~srm4 * rmU - ~(srm1 * rmVH)).norm(), sp<TP>()) << "srmatrix svd";

    rv1 = srbm2.svd(rmU, rmVH);
    rv2.svd(srbm2);

    EXPECT_NEAR(0., (rv1 - rv2).norm(), sp<TP>()) << "srbmatrix svd";
    rv2.svd(srbm2, rmU, rmVH);
    srm1 << basic_srmatrix<TP>{rv1};
    EXPECT_NEAR(0., (srbm2 * ~rmVH - rmU * srm1).norm(), sp<TP>()) << "srbmatrix svd";
    EXPECT_NEAR(0., (~srbm2 * rmU - ~(srm1 * rmVH)).norm(), sp<TP>()) << "srbmatrix svd";


    // test case from Martin
    // http://www.vni.com/products/jmsl/v25/api/com/imsl/math/SVDEx1.html
    basic_rmatrix<TP> A{6, 4};
    A(0, 0) = 1;
    A(0, 1) = 2;
    A(0, 2) = 1;
    A(0, 3) = 4;
    A(1, 0) = 3;
    A(1, 1) = 2;
    A(1, 2) = 1;
    A(1, 3) = 3;
    A(2, 0) = 4;
    A(2, 1) = 3;
    A(2, 2) = 1;
    A(2, 3) = 4;
    A(3, 0) = 2;
    A(3, 1) = 1;
    A(3, 2) = 3;
    A(3, 3) = 1;
    A(4, 0) = 1;
    A(4, 1) = 5;
    A(4, 2) = 2;
    A(4, 3) = 2;
    A(5, 0) = 1;
    A(5, 1) = 2;
    A(5, 2) = 2;
    A(5, 3) = 3;
    basic_srmatrix<TP> U{6}, V{4};
    const basic_rvector<TP> singVal = A.svd(U, V);

    basic_rmatrix<TP> singValM {A};
    singValM.set(0.);
    singValM(0, 0) = singVal(0);
    singValM(1, 1) = singVal(1);
    singValM(2, 2) = singVal(2);
    singValM(3, 3) = singVal(3);

    EXPECT_NEAR(0.,(A * ~V - U * singValM).norm(),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(0.,(~A * U - ~(singValM * V)).norm(),sp<TP>()) << "rmatrix svd";

    EXPECT_NEAR(1.148501791155974e+001, singVal[0],sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(3.269751214412497e+000, singVal[1],sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(2.653356162007834e+000, singVal[2],sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(2.088729672440923e+000, singVal[3],sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.38047558632),std::abs(U(0, 0)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.11967099264),std::abs(U(0, 1)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.43908282438),std::abs(U(0, 2)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.56539958591),std::abs(U(0, 3)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.024311516146),std::abs(U(0, 4)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.5725868611),std::abs(U(0, 5)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.40375371317),std::abs(U(1, 0)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.34511083711),std::abs(U(1, 1)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.05657618529),std::abs(U(1, 2)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.21477557652),std::abs(U(1, 3)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.80890058873),std::abs(U(1, 4)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.11929741721),std::abs(U(1, 5)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.54512048625),std::abs(U(2, 0)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.42926489349),std::abs(U(2, 1)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.051392692809),std::abs(U(2, 2)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.43214416281),std::abs(U(2, 3)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.57232764817),std::abs(U(2, 4)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.040330924871),std::abs(U(2, 5)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.264784294),std::abs(U(3, 0)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.068319525327),std::abs(U(3, 1)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.88386086743),std::abs(U(3, 2)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.21525369818),std::abs(U(3, 3)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.06252092259),std::abs(U(3, 4)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.30621669907),std::abs(U(3, 5)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.4463101123),std::abs(U(4, 0)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.81682762328),std::abs(U(4, 1)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.14189967506),std::abs(U(4, 2)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.32126958427),std::abs(U(4, 3)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.062133782096),std::abs(U(4, 4)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.079935268),std::abs(U(4, 5)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.35462865661),std::abs(U(5, 0)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.10214739916),std::abs(U(5, 1)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.0043184439799),std::abs(U(5, 2)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.54580022185),std::abs(U(5, 3)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-0.098794626562),std::abs(U(5, 4)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(0.74573957611),std::abs(U(5, 5)),sp<TP>()) << "rmatrix svd";

    EXPECT_NEAR(std::abs(-4.442941288423535e-001),std::abs((~V)(0, 0)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-5.580672381903871e-001),std::abs((~V)(1, 0)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-3.243861032062802e-001),std::abs((~V)(2, 0)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-6.212385538433783e-001),std::abs((~V)(3, 0)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(5.555312577999473e-001),std::abs((~V)(0, 1)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-6.542987401123238e-001),std::abs((~V)(1, 1)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-3.513606455925113e-001),std::abs((~V)(2, 1)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(3.739303103834293e-001),std::abs((~V)(3, 1)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-4.353789666739416e-001),std::abs((~V)(0, 2)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(2.774569004588126e-001),std::abs((~V)(1, 2)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-7.320995334295977e-001),std::abs((~V)(2, 2)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(4.444019542237462e-001),std::abs((~V)(3, 2)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-5.517543874418699e-001),std::abs((~V)(0, 3)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(-4.283360651798634e-001),std::abs((~V)(1, 3)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(4.851284633245337e-001),std::abs((~V)(2, 3)),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(std::abs(5.260662365874236e-001),std::abs((~V)(3, 3)),sp<TP>()) << "rmatrix svd";

    basic_rmatrix<TP> rm6{3, 4};
    for (int j = 0; j <= 3; j++) {
        for (int i = 0; i <= 2; i++) {
            rm6(i, j)  = - TP(j * 4 + i);
        }
    }

    rv1.resize(3);
    rv2.resize(3);
    rmU.resize(3);
    rmVH.resize(4);
    rv1 = rm6.svd(rmU, rmVH);
    rv2.svd(rm6, rmU, rmVH);
    EXPECT_TRUE(rv1 == rv2) << "srmatrix svd";

    singValM << rm6;
    singValM.set(0.);
    singValM(0, 0) = rv1(0);
    singValM(1, 1) = rv1(1);
    singValM(2, 2) = rv1(2);
    EXPECT_NEAR(0.,(rm6 * ~rmVH - rmU * singValM).norm(),sp<TP>()) << "rmatrix svd";
    EXPECT_NEAR(0.,(~rm6 * rmU - ~(singValM * rmVH)).norm(),sp<TP>()) << "rmatrix svd";


    rv1.resize(4);
    rv2.resize(4);
    basic_scmatrix<TP,TPC> cmU{4}, cmVH{4};
    rv1 = scm1.svd(cmU, cmVH);
    rv2.svd(scm1, cmU, cmVH);
    EXPECT_TRUE(rv1 == rv2) << "scmatrix svd";
    cv1 << basic_cvector<TP,TPC>{rv1};
    scm << basic_scmatrix<TP,TPC>{cv1};
    EXPECT_NEAR(0.,(scm1 * ~cmVH - cmU * scm).norm(),sp<TP>()) << "scmatrix svd";
    EXPECT_NEAR(0.,(~scm1 * cmU - ~(scm * cmVH)).norm(),sp<TP>()) << "scmatrix svd";

    scbm1(3, 2)=-cr1;
    rv1 = scbm1.svd(cmU, cmVH);
    rv2.svd(scbm1);
    EXPECT_NEAR(0.,(rv1 - rv2).norm(),sp<TP>()) << "scbmatrix svd";
    rv2.svd(scbm1, cmU, cmVH);
    scm1 << basic_scmatrix<TP,TPC>{basic_srmatrix<TP>{rv1}};
    EXPECT_NEAR(0.,(scbm1 * ~cmVH - cmU * scm1).norm(),sp<TP>()) << "scbmatrix svd";
    EXPECT_NEAR(0.,(~scbm1 * cmU - ~(scm1 * cmVH)).norm(),sp<TP>()) << "scbmatrix svd";


    // solvers
    {
        srm4 *=     -1.;
        srm4(2, 2) = 1.;
        srm4(3, 3) = 1.;
        rv.resize(4);
        srm4.normalize();

        rv.solve(srm4, rv1);
        EXPECT_NEAR(0,(srm4 * rv - rv1).norm(),sp<TP>()) << "srmatrix solve";
        rv.solve_tran(srm4, rv1);
        EXPECT_NEAR(0,(rv * srm4 - rv1).norm(),sp<TP>()) << "srmatrix solve transposed";

        rv = srm4.solve(rv1);
        EXPECT_NEAR(0,(srm4 * rv - rv1).norm(),sp<TP>()) << "srmatrix solve";
        rv = srm4.solve_tran(rv1);
        EXPECT_NEAR(0,(rv * srm4 - rv1).norm(),sp<TP>()) << "srmatrix solve transposed";

        rv.solve(srbm2, rv1);
        EXPECT_NEAR(0,(srbm2 * rv - rv1).norm(),sp<TP>()) << "srbmatrix solve";
        rv.solve_tran(srbm2, rv1);
        EXPECT_NEAR(0,(rv * srbm2 - rv1).norm(),sp<TP>()) << "srbmatrix solve transposed";

        rv = srbm2.solve(rv1);
        EXPECT_NEAR(0,(srbm2 * rv - rv1).norm(),sp<TP>()) << "srbmatrix solve";
        rv = srbm2.solve_tran(rv1);
        EXPECT_NEAR(0,(rv * srbm2 - rv1).norm(),sp<TP>()) << "srbmatrix solve transposed";

        basic_rmatrix<TP> rmB{4, 5}, rmX{4, 5};
        rmB.randomize(-3., 4.);

        rmX.solve(srm4, rmB);
        EXPECT_NEAR(0,(srm4 * rmX - rmB).norm(),sp<TP>()) << "srmatrix solve for matrix B";
        rmX.solve_tran(srm4, rmB);
        EXPECT_NEAR(0,(~srm4 * rmX - rmB).norm(),sp<TP>()) << "srmatrix solve for matrix B transposed";
        EXPECT_NEAR(0,(~rmX * srm4 - ~rmB).norm(),sp<TP>()) << "srmatrix solve for matrix B transposed";

        rmX = srm4.solve(rmB);
        EXPECT_NEAR(0,(srm4 * rmX - rmB).norm(),sp<TP>()) << "srmatrix solve for matrix B";
        rmX = srm4.solve_tran(rmB);
        EXPECT_NEAR(0,(~srm4 * rmX - rmB).norm(),sp<TP>()) << "srmatrix solve for matrix B transposed";
        EXPECT_NEAR(0,(~rmX * srm4 - ~rmB).norm(),sp<TP>()) << "srmatrix solve for matrix B transposed";

        rmX.solve(srbm2, rmB);
        EXPECT_NEAR(0,(srbm2 * rmX - rmB).norm(),sp<TP>()) << "srbmatrix solve for matrix B";
        rmX.solve_tran(srbm2, rmB);
        EXPECT_NEAR(0,(~srbm2 * rmX - rmB).norm(),sp<TP>()) << "srbmatrix solve  for matrix B transposed";
        EXPECT_NEAR(0,(~rmX * srbm2 - ~rmB).norm(),sp<TP>()) << "srbmatrix solve  for matrix B transposed";

        rmX = srbm2.solve(rmB);
        EXPECT_NEAR(0,(srbm2 * rmX - rmB).norm(),sp<TP>()) << "srbmatrix solve for matrix B";
        rmX = srbm2.solve_tran(rmB);
        EXPECT_NEAR(0,(~srbm2 * rmX - rmB).norm(),sp<TP>()) << "srbmatrix solve  for matrix B transposed";
        EXPECT_NEAR(0,(~rmX * srbm2 - ~rmB).norm(),sp<TP>()) << "srbmatrix solve  for matrix B transposed";

        scm1.assign(this->c2);
        scm1(2, 2) = TPC(1,-1);
        scm1(3, 3) = TPC(1,-1);
        cv.resize(4);
        cv1.resize(4);

        cv.solve(scm1, cv1);
        EXPECT_NEAR(0,(scm1 * cv - cv1).norm(),sp<TP>()) << "scmatrix solve";
        cv.solve_tran(scm1, cv1);
        EXPECT_NEAR(0,(cv * scm1 - cv1).norm(),sp<TP>()) << "scmatrix solve transposed";
        cv.solve_conj(scm1, cv1);
        EXPECT_NEAR(0,(~scm1 * cv - cv1).norm(),sp<TP>()) << "scmatrix solve conjugated";

        cv = scm1.solve(cv1);
        EXPECT_NEAR(0,(scm1 * cv - cv1).norm(),sp<TP>()) << "scmatrix solve";
        cv = scm1.solve_tran(cv1);
        EXPECT_NEAR(0,(cv * scm1 - cv1).norm(),sp<TP>()) << "scmatrix solve transposed";
        cv = scm1.solve_conj(cv1);
        EXPECT_NEAR(0,(~scm1 * cv - cv1).norm(),sp<TP>()) << "scmatrix solve conjugated";

        cv.solve(scbm1, cv1);
        EXPECT_NEAR(0,(scbm1 * cv - cv1).norm(),sp<TP>()) << "scbmatrix solve";
        cv.solve_tran(scbm1, cv1);
        EXPECT_NEAR(0,(cv * scbm1 - cv1).norm(),sp<TP>()) << "scbmatrix solve transposed";
        cv.solve_conj(scbm1, cv1);
        EXPECT_NEAR(0,(~scbm1 * cv - cv1).norm(),sp<TP>()) << "scbmatrix solve conjugated";

        cv = scbm1.solve(cv1);
        EXPECT_NEAR(0,(scbm1 * cv - cv1).norm(),sp<TP>()) << "scbmatrix solve";
        cv = scbm1.solve_tran(cv1);
        EXPECT_NEAR(0,(cv * scbm1 - cv1).norm(),sp<TP>()) << "scbmatrix solve transposed";
        cv = scbm1.solve_conj(cv1);
        EXPECT_NEAR(0,(~scbm1 * cv - cv1).norm(),sp<TP>()) << "scbmatrix solve conjugated";

        basic_cmatrix<TP,TPC> cmB{4, 5}, cmX{4, 5};
        cmB.randomize_real(-3., 4.);
        cmB.randomize_imag(-5., 2.);
        scm1.randomize_real(-3., 4.);
        scm1.randomize_imag(-2., 4.);
        cmX.solve(scm1, cmB);
        EXPECT_NEAR(0,(scm1 * cmX - cmB).norm(),sp<TP>()) << "scmatrix solve for matrix B";
        cmX = scm1.solve(cmB);
        EXPECT_NEAR(0,(scm1 * cmX - cmB).norm(),sp<TP>()) << "scmatrix solve for matrix B";
        cmX.solve_tran(scm1, cmB);
        EXPECT_NEAR(0,(!cmX * scm1 - !cmB).norm(),sp<TP>()) << "scmatrix solve for matrix B transposed";
        EXPECT_NEAR(0,(!scm1 * cmX - cmB).norm(),sp<TP>()) << "scmatrix solve for matrix B transposed";
        cmX = scm1.solve_tran(cmB);
        EXPECT_NEAR(0,(!cmX * scm1 - !cmB).norm(),sp<TP>()) << "scmatrix solve for matrix B transposed";
        EXPECT_NEAR(0,(!scm1 * cmX - cmB).norm(),sp<TP>()) << "scmatrix solve for matrix B transposed";
        cmX = scm1.solve_conj(cmB);
        EXPECT_NEAR(0,(~cmX * scm1 - ~cmB).norm(),sp<TP>()) << "scmatrix solve for matrix B conjugated";
        EXPECT_NEAR(0,(~scm1 * cmX - cmB).norm(),sp<TP>()) << "scmatrix solve for matrix B conjugated";

        basic_scbmatrix<TP,TPC> scbm{4, 1, 2};
        scbm.randomize_real(-3., 4.);
        scbm.randomize_imag(-2., 4.);
        cmX.solve(scbm, cmB);
        EXPECT_NEAR(0,(scbm * cmX - cmB).norm(),sp<TP>()) << "scbmatrix solve for matrix B";
        cmX = scbm.solve(cmB);
        EXPECT_NEAR(0,(scbm * cmX - cmB).norm(),sp<TP>()) << "scbmatrix solve for matrix B";
        cmX.solve_tran(scbm, cmB);
        EXPECT_NEAR(0,(!cmX * scbm - !cmB).norm(),sp<TP>()) << "scbmatrix solve for matrix B transposed";
        EXPECT_NEAR(0,(!scbm * cmX - cmB).norm(),sp<TP>()) << "scbmatrix solve for matrix B transposed";
        cmX = scbm.solve_tran(cmB);
        EXPECT_NEAR(0,(!cmX * scbm - !cmB).norm(),sp<TP>()) << "scbmatrix solve for matrix B transposed";
        EXPECT_NEAR(0,(!scbm * cmX - cmB).norm(),sp<TP>()) << "scbmatrix solve for matrix B transposed";
        cmX.solve_conj(scbm, cmB);
        EXPECT_NEAR(0,(~cmX * scbm - ~cmB).norm(),sp<TP>()) << "scbmatrix solve for matrix B conjugated";
        EXPECT_NEAR(0,(~scbm * cmX - cmB).norm(),sp<TP>()) << "scbmatrix solve for matrix B conjugated";
        cmX = scbm.solve_conj(cmB);
        EXPECT_NEAR(0,(~cmX * scbm - ~cmB).norm(),sp<TP>()) << "scbmatrix solve for matrix B conjugated";
        EXPECT_NEAR(0,(~scbm * cmX - cmB).norm(),sp<TP>()) << "scbmatrix solve for matrix B conjugated";


        basic_schmatrix<TP,TPC> schm{4};
        schm.randomize_real(-5., 1.);   // 6.1: fixed non-positive definite bug
        schm.randomize_imag(-2., 4.);
        cmX.solve(schm, cmB);
        EXPECT_NEAR(0,(schm * cmX - cmB).norm(),sp<TP>()) << "schmatrix solve for matrix B";
        cmX = schm.solve(cmB);
        EXPECT_NEAR(0,(schm * cmX - cmB).norm(),sp<TP>()) << "schmatrix solve for matrix B";
        cmX.solve_tran(schm, cmB);
        EXPECT_NEAR(0,(!cmX * schm - !cmB).norm(),sp<TP>()) << "schmatrix solve for matrix B transposed";
        EXPECT_NEAR(0,(!schm * cmX - cmB).norm(),sp<TP>()) << "schmatrix solve for matrix B transposed";
        cmX = schm.solve_tran(cmB);
        EXPECT_NEAR(0,(!cmX * schm - !cmB).norm(),sp<TP>()) << "schmatrix solve for matrix B transposed";
        EXPECT_NEAR(0,(!schm * cmX - cmB).norm(),sp<TP>()) << "schmatrix solve for matrix B transposed";
        cmX.solve_conj(schm, cmB);
        EXPECT_NEAR(0,(~cmX * schm - ~cmB).norm(),sp<TP>()) << "schmatrix solve for matrix B conjugated";
        EXPECT_NEAR(0,(~schm * cmX - cmB).norm(),sp<TP>()) << "schmatrix solve for matrix B conjugated";
        cmX = schm.solve_conj(cmB);
        EXPECT_NEAR(0,(~cmX * schm - ~cmB).norm(),sp<TP>()) << "schmatrix solve for matrix B conjugated";
        EXPECT_NEAR(0,(~schm * cmX - cmB).norm(),sp<TP>()) << "schmatrix solve for matrix B conjugated";
    }

//----------------------------------------------------------------------------------

    srm.resize  (3);
    scm.resize  (3);
    cv .resize  (3);
    cv1.resize  (3);
    basic_scmatrix<TP,TPC> scm_{scm.msize()};

    srm(0, 0) = 0.1;  srm(0,1) = 0.2;  srm(0, 2) = 0.1;
    srm(1, 0) = 0.11; srm(1,1) = -2.9; srm(1, 2) = -8.4;
    srm(2, 0) = 0.;   srm(2,1) = 2.91; srm(2, 2) = 8.2;

    cv.eig(srm, scm);
    cv1 = srm.eig(scm_);
//        EXPECT_NEAR(0.,(cv - cv1).norm(),s<TP>()) << "srmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srm} * scm(0) - scm(0) * cv(0)).norm(),sf<TP>()) << "srmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srm} * scm(1) - scm(1) * cv(1)).norm(),sf<TP>()) << "srmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srm} * scm(2) - scm(2) * cv(2)).norm(),sf<TP>()) << "srmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srm} * scm_(0) - scm_(0) * cv1(0)).norm(),sf<TP>()) << "srmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srm} * scm_(1) - scm_(1) * cv1(1)).norm(),sf<TP>()) << "srmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srm} * scm_(2) - scm_(2) * cv1(2)).norm(),sf<TP>()) << "srmatrix eig";

    cv.eig(srm, scm, false);
    cv1 = srm.eig(scm_, false);
//        EXPECT_NEAR(0.,(cv - cv1).norm(),sf<TP>()) << "srmatrix eig, left";
    EXPECT_NEAR(0,(~scm(0) * basic_scmatrix<TP,TPC>{srm} - ~scm(0) * cv(0)).norm(),sf<TP>()) << "srmatrix eig, left";
    EXPECT_NEAR(0,(~scm(1) * basic_scmatrix<TP,TPC>{srm} - ~scm(1) * cv(1)).norm(),sf<TP>()) << "srmatrix eig, left";
    EXPECT_NEAR(0,(~scm(2) * basic_scmatrix<TP,TPC>{srm} - ~scm(2) * cv(2)).norm(),sf<TP>()) << "srmatrix eig, left";
    EXPECT_NEAR(0,(~scm_(0) * basic_scmatrix<TP,TPC>{srm} - ~scm_(0) * cv1(0)).norm(),sf<TP>()) << "srmatrix eig, left";
    EXPECT_NEAR(0,(~scm_(1) * basic_scmatrix<TP,TPC>{srm} - ~scm_(1) * cv1(1)).norm(),sf<TP>()) << "srmatrix eig, left";
    EXPECT_NEAR(0,(~scm_(2) * basic_scmatrix<TP,TPC>{srm} - ~scm_(2) * cv1(2)).norm(),sf<TP>()) << "srmatrix eig, left";

    srm(1, 1) = 2.9;
    cv.eig(srm, scm);
    cv1 = srm.eig(scm_);
//        EXPECT_NEAR(0.,(cv - cv1).norm(),sf<TP>()) << "scmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srm} * scm(0) - scm(0) * cv(0)).norm(),sf<TP>()) << "srmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srm} * scm(1) - scm(1) * cv(1)).norm(),sf<TP>()) << "srmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srm} * scm(2) - scm(2) * cv(2)).norm(),sf<TP>()) << "srmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srm} * scm_(0) - scm_(0) * cv1(0)).norm(),sf<TP>()) << "srmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srm} * scm_(1) - scm_(1) * cv1(1)).norm(),sf<TP>()) << "srmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srm} * scm_(2) - scm_(2) * cv1(2)).norm(),sf<TP>()) << "srmatrix eig";

    cv.eig(srm, scm, false);
    cv1 = srm.eig(scm_, false);
//        EXPECT_NEAR(0.,(cv - cv1).norm(),sf<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(0,(~scm(0) * basic_scmatrix<TP,TPC>{srm} - ~scm(0) * cv(0)).norm(),sf<TP>()) << "srmatrix eig, left";
    EXPECT_NEAR(0,(~scm(1) * basic_scmatrix<TP,TPC>{srm} - ~scm(1) * cv(1)).norm(),sf<TP>()) << "srmatrix eig, left";
    EXPECT_NEAR(0,(~scm(2) * basic_scmatrix<TP,TPC>{srm} - ~scm(2) * cv(2)).norm(),sf<TP>()) << "srmatrix eig, left";
    EXPECT_NEAR(0,(~scm_(0) * basic_scmatrix<TP,TPC>{srm} - ~scm_(0) * cv1(0)).norm(),sf<TP>()) << "srmatrix eig, left";
    EXPECT_NEAR(0,(~scm_(1) * basic_scmatrix<TP,TPC>{srm} - ~scm_(1) * cv1(1)).norm(),sf<TP>()) << "srmatrix eig, left";
    EXPECT_NEAR(0,(~scm_(2) * basic_scmatrix<TP,TPC>{srm} - ~scm_(2) * cv1(2)).norm(),sf<TP>()) << "srmatrix eig, left";

    scm1.resize  (3);
    cv.eig(scm1, scm);
    cv1 = scm1.eig(scm_);
//        EXPECT_NEAR(0.,(cv - cv1).norm(),sp<TP>()) << "scmatrix eig";
    EXPECT_NEAR(0,(scm1 * scm(0) - scm(0) * cv(0)).norm(),sp<TP>()) << "scmatrix eig";
    EXPECT_NEAR(0,(scm1 * scm(1) - scm(1) * cv(1)).norm(),sp<TP>()) << "scmatrix eig";
    EXPECT_NEAR(0,(scm1 * scm(2) - scm(2) * cv(2)).norm(),sp<TP>()) << "scmatrix eig";
    EXPECT_NEAR(0,(scm1 * scm_(0) - scm_(0) * cv1(0)).norm(),sp<TP>()) << "scmatrix eig";
    EXPECT_NEAR(0,(scm1 * scm_(1) - scm_(1) * cv1(1)).norm(),sp<TP>()) << "scmatrix eig";
    EXPECT_NEAR(0,(scm1 * scm_(2) - scm_(2) * cv1(2)).norm(),sp<TP>()) << "scmatrix eig";

    cv.eig(scm1, scm, false);
    cv1 = scm1.eig(scm_ , false);
//        EXPECT_NEAR(0.,(cv - cv1).norm(),sp<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(0,(~scm(0) * scm1 - ~scm(0) * cv(0)).norm(),sp<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(0,(~scm(1) * scm1 - ~scm(1) * cv(1)).norm(),sp<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(0,(~scm(2) * scm1 - ~scm(2) * cv(2)).norm(),sp<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(0,(~scm_(0) * scm1 - ~scm_(0) * cv1(0)).norm(),sp<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(0,(~scm_(1) * scm1 - ~scm_(1) * cv1(1)).norm(),sp<TP>()) << "scmatrix eig, left";
    EXPECT_NEAR(0,(~scm_(2) * scm1 - ~scm_(2) * cv1(2)).norm(),sp<TP>()) << "scmatrix eig, left";

    scm.resize  (4);
    scm_.resize  (4);
    cv .resize  (4);
    cv1.resize  (4);


    cv.eig(srbm2, scm);
    cv1 = srbm2.eig(scm_);
//        EXPECT_NEAR(cv1, cv,sp<TP>()) << "srbmatrix eig vectors";
//        EXPECT_NEAR(0.,(cv - cv1).norm(),sp<TP>()) << "srbmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srbm2} * scm(0) - scm(0) * cv(0)).norm(),sp<TP>()) << "srbmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srbm2} * scm(1) - scm(1) * cv(1)).norm(),sp<TP>()) << "srbmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srbm2} * scm(2) - scm(2) * cv(2)).norm(),sp<TP>()) << "srbmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srbm2} * scm(3) - scm(3) * cv(3)).norm(),sp<TP>()) << "srbmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srbm2} * scm_(0) - scm_(0) * cv1(0)).norm(),sp<TP>()) << "srbmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srbm2} * scm_(1) - scm_(1) * cv1(1)).norm(),sp<TP>()) << "srbmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srbm2} * scm_(2) - scm_(2) * cv1(2)).norm(),sp<TP>()) << "srbmatrix eig";
    EXPECT_NEAR(0,(basic_scmatrix<TP,TPC>{srbm2} * scm_(3) - scm_(3) * cv1(3)).norm(),sp<TP>()) << "srbmatrix eig";

    cv.eig(scbm2, scm);
    cv1 = scbm2.eig(scm_);
//        EXPECT_NEAR(cv1, cv,sp<TP>()) << "scbmatrix eig vectors";
//        EXPECT_NEAR(0.,(cv - cv1).norm(),sp<TP>()) << "scbmatrix eig";
    EXPECT_NEAR(0,(scbm2 * scm(0) - scm(0) * cv(0)).norm(),sp<TP>()) << "scbmatrix eig";
    EXPECT_NEAR(0,(scbm2 * scm(1) - scm(1) * cv(1)).norm(),sp<TP>()) << "scbmatrix eig";
    EXPECT_NEAR(0,(scbm2 * scm(2) - scm(2) * cv(2)).norm(),sp<TP>()) << "scbmatrix eig";
    EXPECT_NEAR(0,(scbm2 * scm(3) - scm(3) * cv(3)).norm(),sp<TP>()) << "scbmatrix eig";
    EXPECT_NEAR(0,(scbm2 * scm_(0) - scm_(0) * cv1(0)).norm(),sp<TP>()) << "scbmatrix eig";
    EXPECT_NEAR(0,(scbm2 * scm_(1) - scm_(1) * cv1(1)).norm(),sp<TP>()) << "scbmatrix eig";
    EXPECT_NEAR(0,(scbm2 * scm_(2) - scm_(2) * cv1(2)).norm(),sp<TP>()) << "scbmatrix eig";
    EXPECT_NEAR(0,(scbm2 * scm_(3) - scm_(3) * cv1(3)).norm(),sp<TP>()) << "scbmatrix eig";


    basic_rvector<TP> b(4), x(4);
    basic_srsmatrix<TP> B{4};
    basic_srmatrix<TP> EV{4};

    B.set(0, 0, 1.00000000000000e+000);
    B.set(1, 0, 5.55244534996568e-001); B.set(1, 1, 2.00000000000000e+000);
    B.set(2, 0, 1.00000000000000e+003); B.set(2,1, 1.38811133749142e+000); B.set(2, 2, 3.00000000000000e+000);
    B.set(3, 0, 1.94335587248799e+000); B.set(3,1, 2.22097813998627e+000); B.set(3, 2, 2.49860040748456e+000); B.set(3, 3, 4.00000000000000e+000);

    b.eig(B, EV);
    x = B.eig();

    EXPECT_NEAR(0.,(x - b).norm(),sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(0,(B * EV(0) - EV(0) * x(0)).norm(),sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(0,(B * EV(1) - EV(1) * x(1)).norm(),sp<TP>()) << "srsmatrix eig";
    EXPECT_NEAR(0,(B * EV(2) - EV(2) * x(2)).norm(),sp<TP>()) << "srsmatrix eig";
    // put schmatrix here

    rv1 = cv1.real();
    rv2 = cv1.imag();
    EXPECT_EQ(0,rv1[3] - cv1(3).real()) << "cvector::real";
    EXPECT_EQ(0,rv2[2] - cv1(2).imag()) << "cvector::imag";

    rm1 = cm1.real();
    rm2 << cm1.imag();
    EXPECT_EQ(0,rm1[2][1] - cm1(2,1).real()) << "cmatrix::real";
    EXPECT_EQ(0, rm2(1,2) - cm1(1,2).imag()) << "cmatrix::imag";

    srm = scm1.real();
    srm1 << scm1.imag();
    EXPECT_EQ(0,srm[2][1]  - scm1(2,1).real()) << "scmatrix::real";
    EXPECT_EQ(0, srm1(1,2) - scm1(1,2).imag()) << "scmatrix::imag";

    cv1.set(TPC(-13.45,1.778));
    cr1 = cv1[1];
    cv << cv1.conj();
    EXPECT_EQ(conj(cr1),cv1(1)) << "cvector::conj";
    cv.conj(cv1);
    EXPECT_EQ(cr1,cv(1)) << "cvector::conj";
    cv1 = ~cv;
    EXPECT_EQ(conj(cr1),cv1(1)) << "cvector ~";

    cr1 = cm1(1, 2);
    cm << cm1.conj();
    EXPECT_EQ(conj(cr1),cm1(2,1)) << "cmatrix::conj";

    cm.conj(cm1);
    EXPECT_EQ(cr1,cm(1,2)) << "cmatrix::conj";
    cm.resize(2, 3);
    cm1.resize(3, 2);
    cm1 = ~cm;
    EXPECT_EQ(conj(cr1),cm1(2,1)) << "cmatrix ~";

    cr1 = scm1(1, 2);
    scm << scm1.conj();
    EXPECT_EQ(conj(cr1),scm1(2,1)) << "scmatrix::conj";
    scm.conj(scm1);
    EXPECT_EQ(cr1,scm(1,2)) << "scmatrix::conj";
    scm1 = ~scm;
    EXPECT_EQ(conj(cr1),scm1(2,1)) << "scmatrix ~";

    cr1 = scbm1(1, 2);
    scbm << scbm1.conj();
    EXPECT_EQ(conj(cr1),scbm1(2,1)) << "scbmatrix::conj";
    scbm.conj(scbm1);
    EXPECT_EQ(cr1,scbm(1,2)) << "scbmatrix::conj";
    scbm1 = ~scbm;
    EXPECT_EQ(conj(cr1),scbm1(2,1)) << "scbmatrix ~";


    r1 = 1.389;
    rv1.set(r1);
    cr1 = cv1[2];
    cv1.assign_real(rv1);
    EXPECT_EQ(r1,cv1(2).real()) << "cvector::assign_real";
    EXPECT_EQ(cr1.imag(),cv1(2).imag()) << "cvector::assign_real";
    cv1.assign_imag(rv1);
    EXPECT_EQ(r1,cv1(1).real()) << "cvector::assign_imag";
    EXPECT_EQ(r1,cv1(1).imag()) << "cvector::assign_imag";

    rm1.resize(3, 2);
    rm1.set(r1);
    cr1 = cm1(2, 1);
    cm1.assign_real(rm1);
    EXPECT_EQ(r1,cm1(2,1).real()) << "cmatrix::assign_real";
    EXPECT_EQ(cr1.imag(), cm1(2,1).imag()) << "cmatrix::assign_real";
    cm1.assign_imag(rm1);
    EXPECT_EQ(r1,cm1(1,1).real()) << "cmatrix::assign_imag";
    EXPECT_EQ(r1,cm1(1,1).imag()) << "cmatrix::assign_imag";


    srm.set(r1);
    cr1 = scm1(2, 1);
    scm1.assign_real(srm);
    EXPECT_EQ(r1,scm1(2,1).real()) << "scmatrix::assign_real";
    EXPECT_EQ(cr1.imag(), scm1(2,1).imag()) << "scmatrix::assign_real";
    scm1.assign_imag(srm);
    EXPECT_EQ(r1,scm1(1,1).real()) << "scmatrix::assign_imag";
    EXPECT_EQ(r1,scm1(1,1).imag()) << "scmatrix::assign_imag";

    srbm.resize(4);
    srbm.set(r1);
    scbm1.resize_lu(1, 2);
    cr1 = scbm1(0, 1);
    scbm1.assign_real(srbm);
    EXPECT_EQ(r1,scbm1(0,1).real()) << "scbmatrix::assign_real";
    EXPECT_EQ(cr1.imag(), scbm1(0,1).imag()) << "scbmatrix::assign_real";
    scbm1.assign_imag(srbm);
    EXPECT_EQ(r1,scbm1(0,1).real()) << "scbmatrix::assign_imag";
    EXPECT_EQ(r1,scbm1(0,1).imag()) << "scbmatrix::assign_imag";

    rm.set(1.);
    rm.normalize();
    rm(1, 2) = -1.1;
    rm(0, 1) = 1.e-9;
    EXPECT_EQ(1,rm.rowofmax()) << "rmatrix::rowofmax";

    EXPECT_EQ(2,rm.colofmax()) << "rmatrix::colofmax";
    EXPECT_EQ(0,rm.rowofmin()) << "rmatrix::rowofmin";
    EXPECT_EQ(1,rm.colofmin()) << "rmatrix::colofmin";
    EXPECT_EQ(2,rm.msize()) << "rmatrix::msize";
    EXPECT_EQ(3,rm.nsize()) << "rmatrix::nsize";
    rm1 << rm.swap_rows(0, 1);
    EXPECT_EQ(TP(-1.1),rm1(0,2)) << "rmatrix::swap_rows";
    rm1.swap_cols(0, 1);
    EXPECT_EQ(TP(1.e-9),rm1(1,0)) << "rmatrix::swap_cols";

    srm.set(1.);
    srm.normalize();
    srm(1, 2) = -1.1;
    srm(0, 1) = 1.e-9;
    EXPECT_EQ(1,srm.rowofmax()) << "srmatrix::rowofmax";
    EXPECT_EQ(2,srm.colofmax()) << "srmatrix::colofmax";
    EXPECT_EQ(0,srm.rowofmin()) << "srmatrix::rowofmin";
    EXPECT_EQ(1,srm.colofmin()) << "srmatrix::colofmin";
    EXPECT_EQ(3,srm.msize()) << "srmatrix::msize";
    EXPECT_EQ(3,srm.nsize()) << "srmatrix::nsize";
    srm1 << srm.swap_rows(0, 1);
    EXPECT_EQ(TP(-1.1),srm1(0,2)) << "srmatrix::swap_rows";
    srm1.swap_cols(0, 1);
    EXPECT_EQ(TP(1.e-9),srm1(1,0)) << "srmatrix::swap_cols";

    cm.set(TPC(1.,1.));
    cm.normalize();
    cm(1,2) = TPC(1.1,1.1);
    cm(0, 1) = TPC(1.e-9,0.);
    EXPECT_EQ(1,cm.rowofmax()) << "cmatrix::rowofmax";
    EXPECT_EQ(2,cm.colofmax()) << "cmatrix::colofmax";
    EXPECT_EQ(0,cm.rowofmin()) << "cmatrix::rowofmin";
    EXPECT_EQ(1,cm.colofmin()) << "cmatrix::colofmin";
    EXPECT_EQ(2,cm.msize()) << "cmatrix::msize";
    EXPECT_EQ(3,cm.nsize()) << "cmatrix::nsize";
    cm1 << cm.swap_rows(0, 1);
    EXPECT_EQ(TPC(1.1,1.1),cm1(0,2)) << "cmatrix::swap_rows";
    cm1.swap_cols(0, 1);
    EXPECT_EQ(TPC(1.e-9,0.),cm1(1,0)) << "cmatrix::swap_cols";

    scm.set(TPC(1.,1.));
    scm.normalize();
    scm(1,2) = TPC(1.1,1.1);
    scm(0, 1) = TPC(1.e-9,0.);
    EXPECT_EQ(1,scm.rowofmax()) << "scmatrix::rowofmax";
    EXPECT_EQ(2,scm.colofmax()) << "scmatrix::colofmax";
    EXPECT_EQ(0,scm.rowofmin()) << "scmatrix::rowofmin";
    EXPECT_EQ(1,scm.colofmin()) << "scmatrix::colofmin";
    EXPECT_EQ(3,scm.msize()) << "scmatrix::msize";
    EXPECT_EQ(3,scm.nsize()) << "scmatrix::nsize";
    scm1 << scm.swap_rows(0, 1);
    EXPECT_EQ(TPC(1.1,1.1),scm1(0,2)) << "scmatrix::swap_rows";
    scm1.swap_cols(0, 1);
    EXPECT_EQ(TPC(1.e-9,0.),scm1(1,0)) << "scmatrix::swap_cols";

    srbm.diag(0) = rv;
    srbm.normalize();
    srbm(1, 2) = -1.1;
    srbm(0, 1) = 1.e-7;

    EXPECT_EQ(1,srbm.rowofmax()) << "srbmatrix::rowofmax";
    EXPECT_EQ(2,srbm.colofmax()) << "srbmatrix::colofmax";
    EXPECT_EQ(2,srbm.rowofmin()) << "srbmatrix::rowofmin";
    EXPECT_EQ(0,srbm.colofmin()) << "srbmatrix::colofmin";
    EXPECT_EQ(4,srbm.msize()) << "srbmatrix::msize";
    EXPECT_EQ(4,srbm.nsize()) << "srbmatrix::nsize";

    scbm.diag(0).set(TPC(1.,1.));
    scbm.normalize();
    scbm(1,2) = TPC(2.,1.);
    scbm(0,1) = TPC(-1.e-10,-1.e-10);
    EXPECT_EQ(1,scbm.rowofmax()) << "scbmatrix::rowofmax";
    EXPECT_EQ(2,scbm.colofmax()) << "scbmatrix::colofmax";
    EXPECT_EQ(3,scbm.rowofmin()) << "scbmatrix::rowofmin";
    EXPECT_EQ(0,scbm.colofmin()) << "scbmatrix::colofmin";
    EXPECT_EQ(4,scbm.msize()) << "scbmatrix::msize";
    EXPECT_EQ(4,scbm.nsize()) << "scbmatrix::nsize";


    for (int i = 0; i < 100; ++i) {
        this->a1[i] = TP(sqrt(i + 1));
        this->a2[i] = TP(i + 1) / TP(10.);
        this->c1[i] = TPC(this->a1[i],this->a2[i]);
    }

    rm2.set(-0.34);
    rm2(1, 2) = 0.;

    EXPECT_EQ(2,rm2.rank()) << "rmatrix::rank";
    rm2.assign(this->a1);
    EXPECT_EQ(4,rm2.rank()) << "rmatrix::rank";

    srm2.assign(this->a1);
    srm2[1].set(0.);
    EXPECT_EQ(2,srm2.rank()) << "srmatrix::rank";
    srm2.diag(0).set(0.);
    EXPECT_EQ(2,srm2.rank()) << "srmatrix::rank";

    cm2.resize(3, 4);
    cm2.assign(this->c1);
    EXPECT_EQ(3,cm2.rank()) << "cmatrix::rank";

    scm2.assign(this->c1);
    EXPECT_EQ(3,scm2.rank()) << "scmatrix::rank";
    scm2.diag(0).set(0.);
    EXPECT_EQ(3,scm2.rank()) << "scmatrix::rank";

    srbm.assign(this->a1);
    EXPECT_EQ(4,srbm.rank()) << "srbmatrix::rank";

    scbm.assign(this->c1);
    EXPECT_EQ(4,scbm.rank()) << "scbmatrix::rank";


    r1 = -8.76;
    srm2.set(r1);
    srm2.diag(1).set(0.);
    EXPECT_EQ(r1,srm2(0,0)) << "srmatrix::diag";
    EXPECT_EQ(0.,srm2(0,1)) << "srmatrix::diag";

    cr1 = TPC(-8.76,-3.6);
    scm2.set(cr1);

    scm2.diag(1).set(0L);
    EXPECT_EQ(std::abs(cr1), std::abs(scm2(0,0))) << "scmatrix::diag";
    EXPECT_EQ(0, std::abs(scm2(0,1))) << "scmatrix::diag";

    srbm.set(r1);
    srbm.diag(1).set(0.);
    EXPECT_EQ(r1,srbm(0,0)) << "srbmatrix::diag";
    EXPECT_EQ(0.,srbm(0,1)) << "srbmatrix::diag";

    scbm.set(cr1);
    scbm.diag(1).set(0.);
    EXPECT_EQ(std::abs(cr1), std::abs(scbm(0,0))) << "scbmatrix::diag";
    EXPECT_EQ(std::abs(TPC(0.,0.)), std::abs(scbm(0,1))) << "srbmatrix::diag";


    srm2.set(r1);
    srm2++;
    EXPECT_EQ(r1 + 1,srm2(0,0)) << "srmatrix++";
    ++srm2;
    EXPECT_EQ(r1 + 2,srm2(1,1)) << "++srmatrix";
    srm2--;
    EXPECT_EQ(r1 + 1,srm2(0,0)) << "srmatrix--";
    --srm2;
    EXPECT_EQ(r1,srm2(1,1)) << "--srmatrix";

    scm2.set(cr1);
    scm2++;
    EXPECT_EQ(std::abs(cr1 + TPC(1)), std::abs(scm2(0,0))) << "scmatrix++";
    ++scm2;
    EXPECT_EQ(std::abs(cr1 + TPC(2)), std::abs(scm2(1,1))) << "++scmatrix";
    scm2--;
    EXPECT_EQ(std::abs(cr1 + TPC(1)), std::abs(scm2(0,0))) << "scmatrix--";
    --scm2;
    EXPECT_EQ(std::abs(cr1), std::abs(scm2(1,1))) << "--scmatrix";

    srbm.set(r1);
    srbm++;
    EXPECT_EQ(r1 + 1,srbm(0,0)) << "srbmatrix++";
    ++srbm;
    EXPECT_EQ(r1 + 2,srbm(1,1)) << "++srbmatrix";
    srbm--;
    EXPECT_EQ(r1 + 1,srbm(0,0)) << "srbmatrix--";
    --srbm;
    EXPECT_EQ(r1,srbm(1,1)) << "--srbmatrix";

    scbm.set(cr1);
    scbm++;
    EXPECT_EQ(std::abs(cr1 + TPC(1)), std::abs(scbm(0,0))) << "scbmatrix++";
    ++scbm;
    EXPECT_EQ(std::abs(cr1 + TPC(2)), std::abs(scbm(1,1))) << "++scbmatrix";
    scbm--;
    EXPECT_EQ(std::abs(cr1 + TPC(1)), std::abs(scbm(0,0))) << "scbmatrix--";
    --scbm;
    EXPECT_EQ(std::abs(cr1), std::abs(scbm(1,1))) << "--scbmatrix";


    srm << srm2.identity();
    EXPECT_EQ(1,srm(0,0)) << "srmatrix::identity";
    EXPECT_EQ(0,srm2(0,1)) << "srmatrix::identity";

    scm << scm2.identity();
    EXPECT_EQ(1,std::abs(scm(0,0))) << "scmatrix::identity";
    EXPECT_EQ(0,std::abs(scm2(0,1))) << "scmatrix::identity";


    srbm << srbm1.identity();
    EXPECT_EQ(1,srbm(0,0)) << "srbmatrix::identity";
    EXPECT_EQ(0,srbm1(0,1)) << "srbmatrix::identity";

    scbm << scbm1.identity();
    EXPECT_EQ(1,std::abs(scbm(0,0))) << "scbmatrix::identity";
    EXPECT_EQ(0,std::abs(scbm1(0,1))) << "scbmatrix::identity";


    rm.assign(this->a2);
    rm1 << rm.transpose();
    EXPECT_EQ(this->a2[1],rm(0,1)) << "rmatrix::transpose";
    rm1.resize(rm.nsize(),rm.msize());
    rm1.transpose(rm);
    EXPECT_EQ(this->a2[2],rm1(0,1)) << "rmatrix::transpose";

    srm.assign(this->a2);
    srm1 << srm.transpose();
    EXPECT_EQ(this->a2[1],srm(0,1)) << "srmatrix::transpose";
    srm1.transpose(srm);
    EXPECT_EQ(this->a2[3],srm1(0,1)) << "srmatrix::transpose";

    srbm.resize_lu(2, 1);
    srbm.assign(this->a2);
    srbm1 << srbm.transpose();
    EXPECT_EQ(this->a2[2],srbm(0,1)) << "srbmatrix::transpose";
    srbm.transpose();
    srbm.assign(this->a2);
    srbm1.transpose(srbm);
    EXPECT_EQ(this->a2[2],srbm1(0,1)) << "srbmatrix::transpose";

    scbm1 = scbm.assign(this->c1);
    scbm1.conj();
    EXPECT_EQ(conj(scbm(1,0)), scbm1(0,1)) << "scbmatrix::conj";
    scbm1.assign(this->c1);
    scbm.conj(scbm1);
    EXPECT_EQ(conj(scbm(2,0)), scbm1(0,2)) << "scbmatrix::conj";
    EXPECT_EQ(conj(scbm(3,0)), scbm1(0,3)) << "scbmatrix::conj";


    srm.resize(3);
    srm1.resize(3);
    srm(0, 0) = 2;    srm(0,1) = -0.8; srm(0, 2) = -0.7;
    srm(1, 0) = -0.4; srm(1,1) = -1;   srm(1, 2) = -0.8;
    srm(2, 0) = -0.6; srm(2,1) = -1.2; srm(2, 2) = -0.9;

    srm1 = srm.exp();
    EXPECT_NEAR(8.484495096274699e+000, srm1(0,0),sp<TP>()) << "srmatrix::exp";
    EXPECT_NEAR(-1.555963610758445e+000, srm1(0,1),sp<TP>()) << "srmatrix::exp";
    EXPECT_NEAR(-1.484978300761370e+000, srm1(0,2),sp<TP>()) << "srmatrix::exp";
    EXPECT_NEAR(-7.330690267073194e-001, srm1(1,0),sp<TP>()) << "srmatrix::exp";
    EXPECT_NEAR(6.959256837027834e-001, srm1(1,1),sp<TP>()) << "srmatrix::exp";
    EXPECT_NEAR(-2.385221030493092e-001, srm1(1,2),sp<TP>()) << "srmatrix::exp";
    EXPECT_NEAR(-1.324167433420492e+000, srm1(2,0),sp<TP>()) << "srmatrix::exp";
    EXPECT_NEAR(-3.128703759020610e-001, srm1(2,1),sp<TP>()) << "srmatrix::exp";
    EXPECT_NEAR(8.267946985957282e-001, srm1(2,2),sp<TP>()) << "srmatrix::exp";

    scm.resize(3);
    scm1.resize(3);
    scm(0, 0) = TPC(1.e-01,2.e-01);
    scm(0, 1) = TPC(3.e-01,4.e-01);
    scm(0, 2) = TPC(5.e-01,6.e-01);
    scm(1, 0) = TPC(1.e+00,1.e+00);
    scm(1, 1) = TPC(0.e+00,0.e+00);
    scm(1, 2) = TPC(0.e+00,0.e+00);
    scm(2, 0) = TPC(-1.e-01,-1.e-01);
    scm(2, 1) = TPC(-3.e-01,-3.e-01);
    scm(2, 2) = TPC(-5.e-01,-5.e-01);

    scm(1, 1)=-cr1;
    scm1 = scm.exp();
    EXPECT_NEAR(std::abs(TPC(-4.816680814596321e+000,-4.855745768190474e+001)), std::abs(scm1(0, 0)), spp<TP>(1.e-10,0.1)) << "scmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-5.878045515841980e+002,-7.809398663068483e+002)), std::abs(scm1(1, 0)), spp<TP>(1.e-10,0.1)) << "scmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(1.111774160999558e+001,3.979363145886382e+001)), std::abs(scm1(2, 0)), spp<TP>(1.e-10,0.1)) << "scmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-1.623884970745376e+002,-2.805917519984524e+002)), std::abs(scm1(0, 1)), spp<TP>(1.e-10,0.1)) << "scmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-5.604582009475869e+003,-3.219074690441815e+003)), std::abs(scm1(1, 1)), spp<TP>(1.e-9,0.1)) << "scmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(1.715815440786858e+002,2.129974004265882e+002)), std::abs(scm1(2, 1)), spp<TP>(1.e-10,0.1)) << "scmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(1.710263520348249e+000,-3.149555947204208e+000)), std::abs(scm1(0, 2)), spp<TP>(1.e-10,0.1)) << "scmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-1.432034221529735e+001,-7.375809596051487e+001)), std::abs(scm1(1, 2)), spp<TP>(1.e-10,0.1)) << "scmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-4.639004479804901e-002,2.814422951041492e+000)), std::abs(scm1(2, 2)), spp<TP>(1.e-10,0.1)) << "scmatrix::exp";


    srbm1.resize(2);
    srbm1.resize_lu(0, 1);
    srbm1(0, 0) = 1.3;
    srbm1(0, 1) = -11.2;
    srbm1(1, 1) = 4.1;

    srm1 << srbm1.exp();
    EXPECT_NEAR(3.669296667619233e+000, srm1(0,0),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(-2.266839637189685e+002, srm1(0,1),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(0, srm1(1,0),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(6.034028759736115e+001, srm1(1,1),sp<TP>()) << "srbmatrix::exp";

    iarray aPivots(3);
    basic_srmatrix<TP> mLU{3}, mLU2{3}, mLo{3}, mUp{3};

    mLU = srm.low_up(aPivots);
    mLU2.low_up(srm, aPivots);
    EXPECT_TRUE(mLU == mLU2) << "srmatrix::low_up";

    mLo.identity();
    mLo(1, 0) = mLU(1, 0);
    mLo(2, 0) = mLU(2, 0);
    mLo(2, 1) = mLU(2, 1);

    mUp(0, 0) = mLU(0, 0);
    mUp(0, 1) = mLU(0, 1);
    mUp(0, 2) = mLU(0, 2);
    mUp(1, 1) = mLU(1, 1);
    mUp(1, 2) = mLU(1, 2);
    mUp(2, 2) = mLU(2, 2);

    mLU = mLo * mUp;
    for (int l = 2; l >= 0; --l) {
        mLU.swap_rows(l,aPivots[l] - 1);
    }
    EXPECT_NEAR(0,(srm - mLU).norminf(),s<TP>()) << "srmatrix::low_up";


    basic_scmatrix<TP,TPC> cmLU{3}, cmLU2{3}, cmLo{3}, cmUp{3};
    cmLU = scm.low_up(aPivots);
    cmLU2.low_up(scm, aPivots);
    EXPECT_TRUE(cmLU == cmLU2) << "scmatrix::low_up";

    cmLo.identity();
    cmLo(1, 0) = cmLU(1, 0);
    cmLo(2, 0) = cmLU(2, 0);
    cmLo(2, 1) = cmLU(2, 1);

    cmUp(0, 0) = cmLU(0, 0);
    cmUp(0, 1) = cmLU(0, 1);
    cmUp(0, 2) = cmLU(0, 2);
    cmUp(1, 1) = cmLU(1, 1);

    cmUp(1, 2) = cmLU(1, 2);
    cmUp(2, 2) = cmLU(2, 2);

    cmLU = cmLo * cmUp;
    for (int l = 2; l >= 0; --l) {
            cmLU.swap_rows(l,aPivots[l] - 1);
    }
    EXPECT_NEAR(0,(scm - cmLU).norminf(),sf<TP>()) << "scmatrix::low_up";


    srm1 << srm.inv();
    EXPECT_NEAR(0,(--(srm1 * srm)).norm(),sf<TP>()) << "srmatrix::inv";
    scm1 << scm.inv();
    EXPECT_NEAR(0,(--(scm1 * scm)).norm(),sf<TP>()) << "scmatrix::inv";


    rv.resize(11);
    rv(0)  = 2.2;
    rv(1)  = 1.3;
    rv(2)  = 1.1;
    rv(3)  = - 0.9;
    rv(4)  = 0.2;
    rv(5)  = - 0.45;
    rv(6)  = 45;
    rv(7)  = - 30;
    rv(8)  = 10;
    rv(9) = 3;
    rv(10) = 3.2;

    srm1.polynom(srm, rv);
    srm2 << srm.polynom(rv);
    EXPECT_TRUE(srm1 == srm2) << "srmatrix::polynom";
    EXPECT_NEAR(1.415106245372072e+004, srm1(0,0),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(8.018578436580816e+002, srm1(0,1),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(1.516628273102821e+002, srm1(0,2),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(6.009153894255998e+002, srm1(1,0),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(8.458618026988163e+003, srm1(1,1),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(6.668127559823842e+003, srm1(1,2),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(-9.855925384439991e+001, srm1(2,0),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(1.020217780733232e+004, srm1(2,1),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(8.075071634102441e+003, srm1(2,2),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";

    rv.resize(3);
    srm1.polynom(srm, rv);
    EXPECT_NEAR(1.001400000000000e+001, srm1(0,0),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(-9.960000000000001e-001, srm1(0,1),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(-1.053000000000000e+000, srm1(0,2),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(-4.320000000000001e-001, srm1(1,0),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(3.408000000000000e+000, srm1(1,1),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(9.400000000000004e-001, srm1(1,2),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(-9.780000000000000e-001, srm1(2,0),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(1.476000000000000e+000, srm1(2,1),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";
    EXPECT_NEAR(3.439000000000000e+000, srm1(2,2),spp<TP>(1.e-10,0.1)) << "srmatrix::polynom";


    cv.resize(11);
    cv(0)  = TPC(2.2,-1);
    cv(1)  = TPC(1.3,-0.6);
    cv(2)  = TPC(1.1,2.3);
    cv(3)  = TPC(-0.9);
    cv(4)  = TPC(0.2,1);
    cv(5)  = TPC(-0.45,2);
    cv(6)  = TPC(45,-17.3);
    cv(7)  = TPC(-30);
    cv(8)  = TPC(10,1.5);
    cv(9) = TPC(3);
    cv(10) = TPC(3.2,-18.9);


    scm(0, 0) = TPC(2.,0.1);
    scm(0, 1) = TPC(-8.e-001,-1);
    scm(0, 2) = TPC(-7.e-001,-2.1);
    scm(1, 0) = TPC(-4.e-001,-0.1);
    scm(1, 1) = TPC(-1.e+000);
    scm(1, 2) = TPC(-8.e-001,3.1);
    scm(2, 0) = TPC(-6.e-001,1);
    scm(2, 1) = TPC(-1.2e+000);
    scm(2, 2) = TPC(-9.e-001,-5.4);

    scm1.polynom(scm, cv);

    EXPECT_NEAR(std::abs(TPC(5.264016832618990e+006,-1.051212804982833e+007)), std::abs(scm1(0, 0)), spp<TP>(1.e-5,100.)) << "scmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(9.386518437203571e+006,-9.534002545240149e+006)), std::abs(scm1(0, 1)), spp<TP>(1.e-5,100.)) << "scmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(2.313187132312614e+007,4.742508767071142e+007)), std::abs(scm1(0, 2)), spp<TP>(1.e-5,100.)) << "scmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(-1.143556158726668e+007,2.626370923270145e+007)), std::abs(scm1(1, 0)), spp<TP>(1.e-5,100.)) << "scmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(-2.183671220461629e+007,2.471364343201455e+007)), std::abs(scm1(1, 1)), spp<TP>(1.e-5,100.)) << "scmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(-6.325599106881835e+007,-1.133746860502928e+008)), std::abs(scm1(1, 2)), spp<TP>(1.e-5,100.)) << "scmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(1.143469364494270e+007,-4.448575764049879e+007)), std::abs(scm1(2, 0)), spp<TP>(1.e-5,100.)) << "scmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(2.832544852276585e+007,-4.473797233313387e+007)), std::abs(scm1(2, 1)), spp<TP>(1.e-5,100.)) << "scmatrix::polynom";
    EXPECT_NEAR(std::abs(TPC(1.291773725514465e+008,1.634454865648127e+008)), std::abs(scm1(2, 2)), spp<TP>(1.e-5,100.)) << "scmatrix::polynom";

    scm2 << scm.polynom(cv);
    EXPECT_NEAR(0.,(scm1.normalize() - scm2.normalize()).norm(),sp<TP>()) << "scmatrix::polynom";

    srbm.resize_lu(1, 0);
    bool bThrew = false;
    try
    {
        srbm(0, 1) = 1.;
    }
    catch(cvmexception e)
    {
        if (e.cause() == CVM_READ_ONLY_ACCESS) bThrew = true;
    }
    EXPECT_EQ(true, bThrew) << "srbmatrix read only exception";

    srbm.diag(0).set(1.);
    srbm.diag(-1).set(1.);

    srm << srbm.exp();

    EXPECT_NEAR(2.718281828459041e+000, srm(0,0),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(0., srm(0,1),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(0., srm(0,2),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(0., srm(0,3),sp<TP>()) << "srbmatrix::exp";

    EXPECT_NEAR(2.718281828459041e+000, srm(1,0),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(2.718281828459041e+000, srm(1,1),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(0., srm(1,2),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(0., srm(1,3),sp<TP>()) << "srbmatrix::exp";

    EXPECT_NEAR(1.359140914229521e+000, srm(2,0),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(2.718281828459041e+000, srm(2,1),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(2.718281828459041e+000, srm(2,2),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(0., srm(2,3),sp<TP>()) << "srbmatrix::exp";

    EXPECT_NEAR(4.530469714098402e-001, srm(3,0),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(1.359140914229521e+000, srm(3,1),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(2.718281828459041e+000, srm(3,2),sp<TP>()) << "srbmatrix::exp";
    EXPECT_NEAR(2.718281828459041e+000, srm(3,3),sp<TP>()) << "srbmatrix::exp";

    srm -= (basic_srmatrix<TP>) srbm;

    scbm.resize_lu(1, 0);
    bThrew = false;
    try
    {
        scbm(0, 1) = 1.;
    }
    catch(cvmexception e)
    {
        if (e.cause() == CVM_READ_ONLY_ACCESS) bThrew = true;
    }
    EXPECT_TRUE(bThrew) << "scbmatrix read only exception";

    scbm.diag(0).set(TPC(1.,1.));
    scbm.diag(-1).set(TPC(1.,1.));

    scm << scbm.exp();

    EXPECT_NEAR(std::abs(TPC(1.468693939915887e+000,2.287355287178844e+000)), std::abs(scm(0, 0)), sp<TP>()) << "scbmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-8.186613472629570e-001,3.756049227094730e+000)), std::abs(scm(1, 0)), sp<TP>()) << "scbmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-2.287355287178843e+000,1.468693939915886e+000)), std::abs(scm(2, 0)), sp<TP>()) << "scbmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-1.252016409031576e+000,-2.728871157543187e-001)), std::abs(scm(3, 0)), sp<TP>()) << "scbmatrix::exp";

    EXPECT_NEAR(std::abs(TPC(0.,0.)), std::abs(scm(0, 1)), sp<TP>()) << "scbmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(1.468693939915887e+000,2.287355287178844e+000)), std::abs(scm(1, 1)), sp<TP>()) << "scbmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-8.186613472629570e-001,3.756049227094730e+000)), std::abs(scm(2, 1)), sp<TP>()) << "scbmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-2.287355287178843e+000,1.468693939915886e+000)), std::abs(scm(3, 1)), sp<TP>()) << "scbmatrix::exp";

    EXPECT_NEAR(std::abs(TPC(0.,0.)), std::abs(scm(0, 2)), sp<TP>()) << "scbmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(0.,0.)), std::abs(scm(1, 2)), sp<TP>()) << "scbmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(1.468693939915887e+000,2.287355287178844e+000)), std::abs(scm(2, 2)), sp<TP>()) << "scbmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(-8.186613472629570e-001,3.756049227094730e+000)), std::abs(scm(3, 2)), sp<TP>()) << "scbmatrix::exp";

    EXPECT_NEAR(std::abs(TPC(0.,0.)), std::abs(scm(0, 3)), sp<TP>()) << "scbmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(0.,0.)), std::abs(scm(1, 3)), sp<TP>()) << "scbmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(0.,0.)), std::abs(scm(2, 3)), sp<TP>()) << "scbmatrix::exp";
    EXPECT_NEAR(std::abs(TPC(1.468693939915887e+000,2.287355287178844e+000)), std::abs(scm(3, 3)), sp<TP>()) << "scbmatrix::exp";

    r1 = 1.217;
    r2 = -.179;
    rv << rv2 << rv1;
    rv2.normalize();

    rv1.gemv(true, srm, r1, rv2, r2);
    rv = rv2 * srm * r1 + r2 * rv;
    EXPECT_NEAR(0.,(rv - rv1).norm(),sp<TP>()) << "rvector::gemv";

    rv = rv1;
    rv1.gemv(false, srm, r1, rv2, r2);
    rv = srm * rv2 * r1 + r2 * rv;
    EXPECT_NEAR(0.,(rv - rv1).norm(),sp<TP>()) << "rvector::gemv";

    rv = rv1;
    rv1.gbmv(true, srbm, r1, rv2, r2);
    rv = rv2 * srbm * r1 + r2 * rv;
    EXPECT_NEAR(0.,(rv - rv1).norm(),sp<TP>()) << "rvector::gbmv";

    rv = rv1;
    rv1.gbmv(false, srbm, r1, rv2, r2);
    rv = srbm * rv2 * r1 + r2 * rv;
    EXPECT_NEAR(0.,(rv - rv1).norm(),sp<TP>()) << "rvector::gbmv";

    cv2 << cv1;
    cv << cv2;
    cv2.normalize();

    cv1.gemv(true, scm, cr1, cv2, cr2);
    cv = cv2 * scm * cr1 + cr2 * cv;
    EXPECT_NEAR(0.,(cv - cv1).norm(),sp<TP>()) << "cvector::gemv";

    cv = cv1;
    cv1.gemv(false, scm, cr1, cv2, cr2);
    cv = scm * cv2 * cr1 + cr2 * cv;
    EXPECT_NEAR(0.,(cv - cv1).norm(),sp<TP>()) << "cvector::gemv";

    cv = cv1;
    cv1.gbmv(true, scbm, cr1, cv2, cr2);
    cv = cv2 * scbm * cr1 + cr2 * cv;
    EXPECT_NEAR(0.,(cv - cv1).norm(),sp<TP>()) << "cvector::gemv";

    cv = cv1;
    cv1.gbmv(false, scbm, cr1, cv2, cr2);

    cv = scbm * cv2 * cr1 + cr2 * cv;
    EXPECT_NEAR(0.,(cv.normalize() - cv1.normalize()).norm(),sp<TP>()) << "cvector::gemv";
}

TYPED_TEST(InitializationTest, TestSubAssignment) {
    basic_rvector<TP> rv(10), rv2(4);
    rv2.randomize(-3., 2.);
    rv.assign(2, rv2);
    EXPECT_EQ(rv[2], rv2[0]) << "rvector subvector assignment";
    EXPECT_EQ(rv[5], rv2[3]) << "rvector subvector assignment";

    basic_cvector<TP,TPC> cv(10), cv2(4);
    cv2.randomize_real(-3., 2.);
    cv2.randomize_imag(-3., 2.);
    cv.assign(2, cv2);
    EXPECT_EQ(cv[2], cv2[0]) << "cvector subvector assignment";
    EXPECT_EQ(cv[5], cv2[3]) << "cvector subvector assignment";

    basic_rmatrix<TP> rm{5,5}, rm2{6,6};
    rm.randomize(-3., 2.);
    rm2.assign(1, 1, rm);
    EXPECT_EQ(rm(0,0), rm2(1,1)) <<  "rmatrix submatrix assignment";
    EXPECT_EQ(rm(1,2), rm2(2,3)) <<  "rmatrix submatrix assignment";

    basic_srmatrix<TP> srm{6};
    srm.randomize(-3., 2.);
    srm.assign(1, 1, rm);
    EXPECT_EQ(rm(0,0), srm(1,1)) <<  "srmatrix submatrix assignment";
    EXPECT_EQ(rm(1,2), srm(2,3)) <<  "srmatrix submatrix assignment";

    basic_cmatrix<TP,TPC> cm{5,5}, cm2{6,6};
    cm.randomize_real(-3., 2.);
    cm.randomize_imag(-3., 2.);
    cm2.assign(1, 1, cm);
    EXPECT_EQ(cm(0,0), cm2(1,1)) <<  "cmatrix submatrix assignment";
    EXPECT_EQ(cm(1,2), cm2(2,3)) <<  "cmatrix submatrix assignment";

    basic_scmatrix<TP,TPC> scm{6};
    scm.randomize_real(-3., 2.);
    scm.randomize_imag(-3., 2.);
    scm.assign(1, 1, cm);
    EXPECT_EQ(cm(0,0), scm(1,1)) <<  "scmatrix submatrix assignment";
    EXPECT_EQ(cm(1,2), scm(2,3)) <<  "scmatrix submatrix assignment";

    basic_srsmatrix<TP> srs1{5}, srs2{3};
    tint ns = srs1.msize();
    srs1.resize(5);
    srs2.randomize(-3., 2.);
    srs1.assign(2,srs2);
    EXPECT_EQ(srs2(0,0), srs1(2,2)) <<  "srsmatrix submatrix assignment";
    EXPECT_EQ(srs2(1,2), srs1(3,4)) <<  "srsmatrix submatrix assignment";
    srs1.resize(ns);

    basic_schmatrix<TP,TPC> sch1{1}, sch2{3};
    ns = sch1.msize();
    sch1.resize(5);
    sch2.randomize_real(-3., 2.);
    sch2.randomize_imag(-3., 2.);
    sch1.assign(2,sch2);
    EXPECT_EQ(sch2(0,0), sch1(2,2)) <<  "schmatrix submatrix assignment";
    EXPECT_EQ(sch2(1,2), sch1(3,4)) <<  "schmatrix submatrix assignment";
    sch1.resize(ns);
}

TYPED_TEST(InitializationTest, TestVectorSetReal) {
    basic_rvector<TP> v(5);
    v.set(3.);
    EXPECT_EQ(TP(3.), v[2]) << "rvector::set";
}

TYPED_TEST(InitializationTest, TestVectorAssignReal) {
    const TP a[] = {1., 2., 3., 4., 5., 6., 7., };
    basic_rvector<TP> v(5);
    basic_rvector<TP> v2(4);
    v.assign(a);
    EXPECT_EQ(TP(3.), v[2]) << "rvector::assign";
    v2.assign(a, 2);
    EXPECT_EQ(TP(5.), v2[2]) << "rvector::assign";
}

TYPED_TEST(InitializationTest, TestVectorIntRealCtr) {
    basic_rvector<TP> v(5, 1.5);
    EXPECT_EQ(TP(1.5), v[2]) << "rvector(int,real)";
}

TYPED_TEST(InitializationTest, TestVectorImagComplex) {
    basic_cvector<TP,TPC> vc(3);
    vc.set(TPC(1.,1.));
    vc.imag()(0) = 7.77;
    EXPECT_EQ(std::abs(TPC(1.,7.77)), std::abs(vc[0])) << "cvector::imag";
    EXPECT_EQ(std::abs(TPC(1.,1.)), std::abs(vc[1])) << "cvector::imag";
}

TYPED_TEST(InitializationTest, TestVectorRealComplex) {
    basic_cvector<TP,TPC> vc(3);
    vc.set(TPC(1.,1.));
    vc.real()(0) = 7.77;
    EXPECT_EQ(std::abs(TPC(7.77,1.)), std::abs(vc[0])) << "cvector::real";
    EXPECT_EQ(std::abs(TPC(1.,1.)), std::abs(vc[1])) << "cvector::real";
}

TYPED_TEST(InitializationTest, TestVectorSetRealComplex) {
    basic_cvector<TP,TPC> v(3);
    v.set_real(1.);
    EXPECT_EQ(std::abs(TPC(1.,0.)), std::abs(v[1])) << "cvector::set_real";
}

TYPED_TEST(InitializationTest, TestVectorAssignImagComplex) {
    basic_rvector<TP> v(3);
    basic_cvector<TP,TPC> vc(3);
    v(0) = 1.;
    v(1) = 2.;
    v(2) = 3.;
    vc.assign_imag(v);
    EXPECT_EQ(std::abs(TPC(0.,2.)), std::abs(vc[1])) << "cvector::assign_imag";
}

TYPED_TEST(InitializationTest, TestMatrixLdReal) {
    basic_rmatrix<TP> m(100, 200);
    basic_srmatrix<TP> ms(m, 30, 40, 5); // 5x5 submatrix
    EXPECT_EQ(100, ms.ld()) << "srmatrix::ld";
}

TYPED_TEST(InitializationTest, TestTransposeReal) {
    basic_rmatrix<TP> rm{7, 6}, rm2{6, 7};
    rm.randomize(-3., 5.);
    rm2.transpose(rm);
    EXPECT_NEAR(0.,(rm - ~rm2).norm(),s<TP>()) << "rmatrix transposed";
    EXPECT_NEAR(rm2(1,1), rm(1, 1),s<TP>()) << "rmatrix transposed";
    EXPECT_NEAR(rm2(2,1), rm(1, 2),s<TP>()) << "rmatrix transposed";
    rm2.transpose();
    EXPECT_NEAR(0.,(rm - rm2).norm(),s<TP>()) << "rmatrix transposed";

    basic_srmatrix<TP> srm{7}, srm2{7};
    srm.randomize(-3., 5.);
    srm2.transpose(srm);
    EXPECT_NEAR(0.,(srm - ~srm2).norm(),s<TP>()) << "srmatrix transposed";
    EXPECT_NEAR(srm2(1,1), srm(1, 1),s<TP>()) << "srmatrix transposed";
    EXPECT_NEAR(srm2(2,1), srm(1, 2),s<TP>()) << "srmatrix transposed";
    srm2.transpose();
    EXPECT_NEAR(0.,(srm - srm2).norm(),s<TP>()) << "srmatrix transposed";

    basic_srbmatrix<TP> srbm{7, 1, 2}, srbm2{7, 2, 1};
    srbm.randomize(-3., 5.);
    srbm2.transpose(srbm);
    EXPECT_NEAR(0.,(srbm - ~srbm2).norm(),s<TP>()) << "srbmatrix transposed";
    EXPECT_NEAR(srbm2(1,1), srbm(1, 1),s<TP>()) << "srbmatrix transposed";
    EXPECT_NEAR(srbm2(2,1), srbm(1, 2),s<TP>()) << "srbmatrix transposed";
    srbm2.transpose();
    EXPECT_NEAR(0.,(srbm - srbm2).norm(),s<TP>()) << "srbmatrix transposed";

    basic_srsmatrix<TP> srsm{7}, srsm2{7};
    srsm.randomize(-3., 5.);
    srsm2.transpose(srsm);
    EXPECT_NEAR(0.,(srsm - ~srsm2).norm(),s<TP>()) << "srsmatrix transposed";
    EXPECT_NEAR(srsm2(1,1), srsm(1, 1),s<TP>()) << "srsmatrix transposed";
    EXPECT_NEAR(srsm2(2,1), srsm(1, 2),s<TP>()) << "srsmatrix transposed";
    srsm2.transpose();
    EXPECT_NEAR(0.,(srsm - srsm2).norm(),s<TP>()) << "srsmatrix transposed";
}

TYPED_TEST(InitializationTest, TestTransposeComplex) {
    basic_cmatrix<TP,TPC> cm{7, 6}, cm2{6, 7};
    cm.randomize_real(-3., 5.);
    cm.randomize_imag(-5., 4.);
    cm2.transpose(cm);
    EXPECT_NEAR(0.,(cm - !cm2).norm(), s<TP>()) << "cmatrix transposed";
    EXPECT_NEAR(std::abs(cm2(1,1)), std::abs(cm(1,1)), s<TP>()) << "cmatrix transposed";
    EXPECT_NEAR(std::abs(cm2(2,1)), std::abs(cm(1,2)), s<TP>()) << "cmatrix transposed";
    cm2.transpose();
    EXPECT_NEAR(0.,(cm - cm2).norm(),s<TP>()) << "cmatrix transposed";
    cm2.transpose();
    cm2.conj(cm);
    EXPECT_NEAR(0.,(cm - ~cm2).norm(),s<TP>()) << "cmatrix conjugated";
    EXPECT_NEAR(std::abs(std::conj(cm2(1,1))), std::abs(cm(1,1)), s<TP>()) << "cmatrix conjugated";
    EXPECT_NEAR(std::abs(std::conj(cm2(2,1))), std::abs(cm(1,2)), s<TP>()) << "cmatrix conjugated";
    cm2.conj();
    EXPECT_NEAR(0.,(cm - cm2).norm(),s<TP>()) << "cmatrix conjugated";

    basic_scmatrix<TP,TPC> scm{7}, scm2{7};
    scm.randomize_real(-3., 5.);
    scm.randomize_imag(-5., 3.);
    scm2.transpose(scm);
    EXPECT_NEAR(0.,(scm - !scm2).norm(),s<TP>()) << "scmatrix transposed";
    EXPECT_NEAR(std::abs(scm2(1,1)), std::abs(scm(1,1)), s<TP>()) << "scmatrix transposed";
    EXPECT_NEAR(std::abs(scm2(2,1)), std::abs(scm(1,2)), s<TP>()) << "scmatrix transposed";
    scm2.transpose();
    EXPECT_NEAR(0.,(scm - scm2).norm(),s<TP>()) << "scmatrix transposed";
    scm2.conj(scm);
    EXPECT_NEAR(0.,(scm - ~scm2).norm(),s<TP>()) << "scmatrix conjugated";
    EXPECT_NEAR(std::abs(std::conj(scm2(1,1))), std::abs(scm(1,1)), s<TP>()) << "scmatrix conjugated";
    EXPECT_NEAR(std::abs(std::conj(scm2(2,1))), std::abs(scm(1,2)), s<TP>()) << "scmatrix conjugated";
    scm2.conj();
    EXPECT_NEAR(0.,(scm - scm2).norm(),s<TP>()) << "scmatrix conjugated";

    basic_scbmatrix<TP,TPC> scbm{7, 1, 2}, scbm2{7, 2, 1};
    scbm.randomize_real(-3., 5.);
    scbm.randomize_imag(-4., 3.);
    scbm2.transpose(scbm);
    EXPECT_NEAR(0.,(scbm - !scbm2).norm(),s<TP>()) << "scbmatrix transposed";
    EXPECT_NEAR(std::abs(scbm2(1,1)), std::abs(scbm(1,1)), s<TP>()) << "scbmatrix transposed";
    EXPECT_NEAR(std::abs(scbm2(2,1)), std::abs(scbm(1,2)), s<TP>()) << "scbmatrix transposed";
    scbm2.transpose();
    EXPECT_NEAR(0.,(scbm - scbm2).norm(),s<TP>()) << "scbmatrix transposed";
    scbm2.transpose();
    scbm2.conj(scbm);
    EXPECT_NEAR(0.,(scbm - ~scbm2).norm(),s<TP>()) << "scbmatrix conjugated";
    EXPECT_NEAR(std::abs(std::conj(scbm2(1,1))), std::abs(scbm(1,1)), s<TP>()) << "scbmatrix conjugated";
    EXPECT_NEAR(std::abs(std::conj(scbm2(2,1))), std::abs(scbm(1,2)), s<TP>()) << "scbmatrix conjugated";
    scbm2.conj();
    EXPECT_NEAR(0.,(scbm - scbm2).norm(),s<TP>()) << "scbmatrix conjugated";

    basic_schmatrix<TP,TPC> schm{5};
    schm.randomize_real(-2., 3.);
    schm.randomize_imag(-3., 2.);
    basic_schmatrix<TP,TPC> schmc = ~schm;
    EXPECT_NEAR(0.,(schmc - schm).norm(),s<TP>()) << "schmatrix conjugated";
    ((basic_cmatrix<TP,TPC>)schmc).conj();
    EXPECT_NEAR(0.,(schmc - schm).norm(),s<TP>()) << "schmatrix conjugated";
    ((basic_scmatrix<TP,TPC>)schmc).conj();
    EXPECT_NEAR(0.,(schmc - schm).norm(),s<TP>()) << "schmatrix conjugated";
    basic_scmatrix<TP,TPC> scmc = ~((basic_scmatrix<TP,TPC>)schm);
    EXPECT_NEAR(0.,(scmc - schm).norm(),s<TP>()) << "schmatrix conjugated";
    schmc = !schm;
    schmc.transpose(schm);
    EXPECT_NEAR(0.,(schmc - !schm).norm(),s<TP>()) << "schmatrix transposed";
    EXPECT_NEAR(std::abs(schm(1,1)), std::abs(schmc(1,1)), s<TP>()) << "schmatrix transposed";
    EXPECT_NEAR(std::abs(schm(2,1)), std::abs(schmc(1,2)), s<TP>()) << "schmatrix transposed";
    schmc.transpose();
    EXPECT_NEAR(0.,(schmc - schm).norm(),s<TP>()) << "schmatrix transposed";
}

TYPED_TEST(InitializationTest, TestMainDiagHermitianComplex) {
    TP a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
              0., 3., -1., -2., 0., -3., 3., 0.};
    basic_schmatrix<TP,TPC> m((TPC*)a, 3);
    basic_rvector<TP> v(3);
    v.set(7.7);
    m.set_main_diag(v);
    EXPECT_EQ(TPC(7.7,0.), m(0, 0)) << "schmatrix::set_main_diag";
    EXPECT_EQ(TPC(7.7,0.), m(2, 2)) << "schmatrix::set_main_diag";
    EXPECT_EQ(TPC(2.,-1.), m(0, 1)) << "schmatrix::set_main_diag";
    EXPECT_EQ(TPC(2.,1.), m(1, 0)) << "schmatrix::set_main_diag";
}

TYPED_TEST(InitializationTest, TestDiagHermitianComplex) {
    TP a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
              0., 3., -1., -2., 0., -3., 3., 0.};
    basic_schmatrix<TP,TPC> m((TPC*)a, 3);
    basic_cvector<TP,TPC> v(2);
    v.set(TPC(7.,7.));
    m.set_diag(1, v);
    EXPECT_EQ(TPC(7.,7.), m(0, 1)) << "schmatrix::set_diag";
    EXPECT_EQ(TPC(7.,-7.), m(1, 0)) << "schmatrix::set_diag";
    EXPECT_EQ(TPC(2.,0.), m(1, 1)) << "schmatrix::set_diag";
}

TYPED_TEST(InitializationTest, TestResizeBandComplex) {
    TP a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12.};
    basic_scbmatrix<TP,TPC> m((TPC*)a, 3, 1, 0);
    m.resize_lu(0, 1);
    m.diag(1).set(TPC(9.,9.));
    EXPECT_EQ(TPC(9.,9.), m(0, 1)) << "scbmatrix::resize_lu";
    EXPECT_EQ(TPC(0.,0.), m(1, 0)) << "scbmatrix::resize_lu";
    EXPECT_EQ(TPC(9.,10.), m(2, 2)) << "scbmatrix::resize_lu";
}

TYPED_TEST(InitializationTest, TestRealImagBandComplex) {
    TP a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12.};
    basic_scbmatrix<TP,TPC> m((TPC*)a, 3, 1, 0);
    EXPECT_NEAR(TP(5.), m.real()(1, 1), s<TP>()) << "scbmatrix::real";
    EXPECT_NEAR(TP(6.), m.imag()(1, 1), s<TP>()) << "scbmatrix::imag";
}

TYPED_TEST(InitializationTest, TestBoolCtrBandComplex) {
    TP a[] = {1., 2., 3., 4., 5., 6., 7., 8.};
    const basic_srbmatrix<TP> m(a, 4, 1, 0);
    basic_scbmatrix<TP,TPC> mr(m), mi(m, false);
    EXPECT_EQ(TPC(6.,0.), mr(3, 2)) << "basic_scbmatrix<TP,TPC>(srbmatrix, bool)";
    EXPECT_EQ(TPC(0.,0.), mr(0, 3)) << "basic_scbmatrix<TP,TPC>(srbmatrix, bool)";
    EXPECT_EQ(TPC(0.,6.), mi(3, 2)) << "basic_scbmatrix<TP,TPC>(srbmatrix, bool)";
    EXPECT_EQ(TPC(0.,0.), mi(0, 3)) << "basic_scbmatrix<TP,TPC>(srbmatrix, bool)";
}

TYPED_TEST(InitializationTest, TestRealRealCtrBandComplex) {
    basic_srbmatrix<TP> mr(4, 1, 0), mi(4, 1, 0);
    mr.set(1.);
    mi.set(2.);
    const basic_scbmatrix<TP,TPC> m(mr, mi);
    EXPECT_EQ(TPC(1.,2.), m(1, 0)) << "basic_scbmatrix<TP,TPC>(srbmatrix, srbmatrix)";
    EXPECT_EQ(TPC(0.,0.), m(0, 1)) << "basic_scbmatrix<TP,TPC>(srbmatrix, srbmatrix)";
}

TYPED_TEST(InitializationTest, TestDiagReal) {
    TP a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
    basic_rmatrix<TP>  m(2, 3);
    const basic_srmatrix<TP> ms(a, 3);
    m.diag(-1).set(1.);
    m.diag(0).set(2.);
    m.diag(1).set(3.);
    m.diag(2).set(4.);
    EXPECT_EQ(TP(3.), m(0, 1)) << "rmatrix::diag";
    EXPECT_EQ(TP(4.), m(0, 2)) << "rmatrix::diag";
    EXPECT_EQ(TP(1.), m(1, 0)) << "rmatrix::diag";
    EXPECT_EQ(TP(5.), ms.diag(0)(1)) << "rmatrix::diag";
}

TYPED_TEST(InitializationTest, TestDiagComplex) {
    TP a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12., 13., 14., 15., 16., 17., 18.};
    basic_cmatrix<TP,TPC> m(2, 3);
    const basic_scmatrix<TP,TPC> ms((TPC*)a, 3);
    m.diag(-1).set(TPC(1.,1.));
    m.diag(0).set(TPC(2.,2.));
    m.diag(1).set(TPC(3.,3.));
    m.diag(2).set(TPC(4.,4.));
    EXPECT_EQ(TPC(3.,3.), m(0, 1)) << "cmatrix::diag";
    EXPECT_EQ(TPC(3.,3.), m(1, 2)) << "cmatrix::diag";
    EXPECT_EQ(TPC(9.,10.), ms.diag(0)[1]) << "cmatrix::diag";
}

TYPED_TEST(InitializationTest, TestBandAssignReal) {
    basic_srmatrix<TP> s(9);
    basic_srbmatrix<TP> m(3, 0, 1);
    for(int i = 1; i <= 9; ++i) {
        s[i - 1].set(TP(i));
    }
    m.assign(s(8));     // should be 1, 2, ..9
    EXPECT_EQ(TP(2.), m(0, 0)) << "rmatrix.assign(vector)";
    EXPECT_EQ(TP(3.), m(0, 1)) << "rmatrix.assign(vector)";
    EXPECT_EQ(TP(6.), m(2, 2)) << "rmatrix.assign(vector)";
    m.assign(s[8]);     // should be 9, 9, ..9
    EXPECT_EQ(TP(9.), m(0, 0)) << "rmatrix.assign(vector)";
    EXPECT_EQ(TP(9.), m(0, 1)) << "rmatrix.assign(vector)";
    EXPECT_EQ(TP(9.), m(2, 2)) << "rmatrix.assign(vector)";
}

TYPED_TEST(InitializationTest, TestBandAssignComplex) {
    basic_scmatrix<TP,TPC> s(9);
    basic_scbmatrix<TP,TPC> m(3, 1, 1);
    for (int i = 1; i <= 9; ++i) {
        s[i - 1].set(TPC(TP(i), TP(-i)));
    }
    m.assign(s(8));     // should be 1, 2, ..9
    EXPECT_EQ(TPC(2., -2.), m(0, 0)) << "scbmatrix.assign(vector)";
    EXPECT_EQ(TPC(4., -4.), m(0, 1)) << "scbmatrix.assign(vector)";
    EXPECT_EQ(TPC(8., -8.), m(2, 2)) << "scbmatrix.assign(vector)";
    EXPECT_EQ(TPC(0.,0.), m(0, 2)) << "scbmatrix.assign(vector)";
    m.assign(s[8]);     // should be 9, 9, ..9
    EXPECT_EQ(TPC(9., -9.), m(0, 0)) << "scbmatrix.assign(vector)";
    EXPECT_EQ(TPC(9., -9.), m(0, 1)) << "scbmatrix.assign(vector)";
    EXPECT_EQ(TPC(9., -9.), m(2, 2)) << "scbmatrix.assign(vector)";
    EXPECT_EQ(TPC(0.,0.), m(0, 2)) << "scbmatrix.assign(vector)";
}

TYPED_TEST(InitializationTest, TestAssignReal) {
    basic_srmatrix<TP> s(9);
    basic_rmatrix<TP> mbig(30, 30);
    basic_rmatrix<TP> m(mbig, 4, 7, 3, 3);
    for (int i = 1; i <= 9; ++i) {
        s[i - 1].set(TP(i));
    }
    m.assign(s(8));     // should be 1, 2, ..9
    EXPECT_EQ(TP(1.), m(0, 0)) << "rmatrix.assign(vector)";
    EXPECT_EQ(TP(4.), m(0, 1)) << "rmatrix.assign(vector)";
    EXPECT_EQ(TP(9.), m(2, 2)) << "rmatrix.assign(vector)";
    m.assign(s[8]);     // should be 9, 9, ..9
    EXPECT_EQ(TP(9.), m(0, 0)) << "rmatrix.assign(vector)";
    EXPECT_EQ(TP(9.), m(0, 1)) << "rmatrix.assign(vector)";
    EXPECT_EQ(TP(9.), m(2, 2)) << "rmatrix.assign(vector)";
}

TYPED_TEST(InitializationTest, TestSquareAssignReal) {
    basic_srmatrix<TP> s(9);
    basic_srmatrix<TP> mbig(20);
    basic_srmatrix<TP> m(mbig, 4, 7, 3);
    for (int i = 1; i <= 9; ++i) {
        s[i - 1].set(TP(i));
    }
    m.assign(s(8));     // should be 1, 2, ..9
    EXPECT_EQ(TP(1.), m(0, 0)) << "srmatrix.assign(vector)";
    EXPECT_EQ(TP(4.), m(0, 1)) << "srmatrix.assign(vector)";
    EXPECT_EQ(TP(9.), m(2, 2)) << "srmatrix.assign(vector)";
    m.assign(s[8]);     // should be 9, 9, ..9
    EXPECT_EQ(TP(9.), m(0, 0)) << "srmatrix.assign(vector)";
    EXPECT_EQ(TP(9.), m(0, 1)) << "srmatrix.assign(vector)";
    EXPECT_EQ(TP(9.), m(2, 2)) << "srmatrix.assign(vector)";
}

TYPED_TEST(InitializationTest, TestAssignComplex) {
    basic_scmatrix<TP,TPC> s(9);
    basic_cmatrix<TP,TPC> mbig(30, 30);
    basic_cmatrix<TP,TPC> m(mbig, 4, 7, 3, 3);
    for (int i = 1; i <= 9; ++i) {
        s[i - 1].set(TPC(TP(i),TP(-i)));
    }
    m.assign(s(8));     // should be 1, 2, ..9
    EXPECT_EQ(TPC(1., -1.), m(0, 0)) << "cmatrix.assign(vector)";
    EXPECT_EQ(TPC(4., -4.), m(0, 1)) << "cmatrix.assign(vector)";
    EXPECT_EQ(TPC(9., -9.), m(2, 2)) << "cmatrix.assign(vector)";
    m.assign(s[8]);     // should be 9, 9, ..9
    EXPECT_EQ(TPC(9., -9.), m(0, 0)) << "cmatrix.assign(vector)";
    EXPECT_EQ(TPC(9., -9.), m(0, 1)) << "cmatrix.assign(vector)";
    EXPECT_EQ(TPC(9., -9.), m(2, 2)) << "cmatrix.assign(vector)";
}

TYPED_TEST(InitializationTest, TestSymmetricAssignReal) {
    basic_srmatrix<TP> s(9);
    basic_srsmatrix<TP> m(3);
    s(8, 1) = 1.;
    s(8, 3) = 1.;
    s(8, 8) = 5.;
    m.assign(s[8]);
    EXPECT_EQ(TP(0.), m(0, 0)) << "srsmatrix.assign(vector)";
    EXPECT_EQ(TP(1.), m(0, 1)) << "srsmatrix.assign(vector)";
    EXPECT_EQ(TP(1.), m(1, 0)) << "srsmatrix.assign(vector)";
    EXPECT_EQ(TP(5.), m(2, 2)) << "srsmatrix.assign(vector)";
}

// 5.4.2
TYPED_TEST(InitializationTest, TestLdComplex) {
    // bug fix check
    basic_cmatrix<TP,TPC> a(3, 4);
    EXPECT_EQ(3, a.ld()) << "a.ld()";
    a.resize(0, 0);
    EXPECT_EQ(0, a.ld()) << "a.ld()";
}

TYPED_TEST(InitializationTest, TestVectorOfVectorComplex) {
    std::vector<basic_cvector<TP,TPC>> vcv;
    vcv.reserve(5);
    vcv.push_back(basic_cvector<TP,TPC>(10));
    vcv.push_back(basic_cvector<TP,TPC>());
    vcv[0][0] = TPC(1.,-1.);
    EXPECT_EQ(TPC(1., -1.), vcv[0](0)) << "std::vector<cvector>[][]";
}

TYPED_TEST(InitializationTest, TestVectorOfMatrixReal) {
    std::vector<basic_srmatrix<TP>> vcm;
    vcm.reserve(5);
    vcm.push_back(basic_srmatrix<TP>(10));
    vcm.push_back(basic_srmatrix<TP>());
    vcm[0][0][1] = 7.77;
    EXPECT_EQ(TP(7.77), vcm[0](0, 1)) << "std::vector<srmatrix>[][][]";
}

TYPED_TEST(InitializationTest, TestVectorOfMatrixComplex) {
    std::vector<basic_cmatrix<TP,TPC>> vcm;
    vcm.reserve(5);
    vcm.push_back(basic_cmatrix<TP,TPC>(10, 20));
    vcm[0][0][1] = TPC(1.,-1.);
    EXPECT_EQ(TPC(1., -1.), vcm[0](0, 1)) << "std::vector<cmatrix>[][][]";
}

// 5.5.1 coverage
TYPED_TEST(InitializationTest, TestCoverage) {
    basic_cvector<TP,TPC> v(5);
    v.set_real(3.45);
    v.set_imag(-4.17);
    EXPECT_EQ(TPC(TP(3.45), TP(-4.17)), v(0)) << "cvector set_real set_image";
    EXPECT_EQ(TPC(TP(3.45), TP(-4.17)), v[4]) << "cvector set_real set_image";

    basic_cmatrix<TP,TPC> m(4, 5);
    m.set_real(3.45);
    m.set_imag(-4.17);
    EXPECT_EQ(TPC(TP(3.45), TP(-4.17)), m(0, 2)) << "cmatrix set_real set_image";
    EXPECT_EQ(TPC(TP(3.45), TP(-4.17)), m[3][4]) << "cmatrix set_real set_image";

    basic_scmatrix<TP,TPC> sm(5);
    sm.set_real(3.45);
    sm.set_imag(-4.17);
    EXPECT_EQ(TPC(TP(3.45), TP(-4.17)), sm(0, 2)) << "scmatrix set_real set_image";
    EXPECT_EQ(TPC(TP(3.45), TP(-4.17)), sm[4][4]) << "scmatrix set_real set_image";

    basic_scbmatrix<TP,TPC> bm(5, 1, 2);
    bm.set_real(3.45);
    bm.set_imag(-4.17);
    EXPECT_EQ(TPC(TP(3.45), TP(-4.17)), bm(0, 2)) << "scbmatrix set_real set_image";
    EXPECT_EQ(TPC(TP(3.45), TP(-4.17)), bm[4][4]) << "scbmatrix set_real set_image";

    basic_schmatrix<TP,TPC> hm(5);
    hm.set_real(3.45);
    EXPECT_EQ(TPC(3.45,0.), hm(0, 2)) << "schmatrix set_real";

    bool ex = false;
    try {
        hm.set_imag(-4.17);
    }
    catch(cvmexception&)
    {
        ex = true;
    }
    EXPECT_TRUE(ex) << "schmatrix set_image not allowed";
}
