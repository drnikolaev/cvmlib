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
class FunctionalTest : public ::testing::Test {
protected:
    FunctionalTest() {}
    virtual ~FunctionalTest() {}
};

TYPED_TEST_CASE(FunctionalTest, TestTypes);

// 8.0 move
TYPED_TEST(FunctionalTest, TestMoveFVector) {
    string_array sa;
    sa.push_back ("{x, z} sign(x+2)");
    sa.push_back ("{x, z} z+3");

    basic_rfvector<TP> fa(sa);
    basic_rfvector<TP> fb(fa+fa);

    TP x[2];
    TP y[2] = {};

    x[0] = -2.1;
    x[1] = 8.8;
    fb.value(x, y);

    EXPECT_EQ(TP(-2.), y[0]) << "rfvector - value";
    EXPECT_EQ(TP(23.6), y[1]) << "rfvector - value";

    basic_rvector<TP> yv = fb(x);
    EXPECT_EQ(TP(-2.), yv[CVM0]) << "rfvector - value";
    EXPECT_EQ(TP(23.6), yv[CVM0+1]) << "rfvector - value";

    basic_rfvector<TP> fc = fb + fa;
    EXPECT_STREQ("sign(x+2)*3", fc.simp()[0].format().c_str()) << "a + b - format()";
    EXPECT_STREQ("(z+3)*3", fc.simp()[1].format().c_str()) << "a + b - format()";
}

// 8.0 move
TYPED_TEST(FunctionalTest, TestMoveFMatrix) {
    string_array sa;
    sa.push_back ("{x, z} sign(x+2)");
    sa.push_back ("{x, z} z+3");
    sa.push_back ("{x, z} sign(x-2)");
    sa.push_back ("{x, z} z-3");

    basic_rfmatrix<TP> fa(2, 2, sa);
    basic_rfmatrix<TP> fb(fa+fa);

    TP x[2];
    x[0] = -2.1;
    x[1] = 8.8;

    basic_rmatrix<TP> ym = fb(x);
    EXPECT_EQ(TP(-2.), ym(CVM0, CVM0)) << "rfmatrix - value";
    EXPECT_EQ(TP(23.6), ym(CVM0+1, CVM0)) << "rfmatrix - value";

    basic_rfmatrix<TP> fc = fb - fa;
    EXPECT_EQ("sign(x+2)", fc.simp().at(TP(0), 0).format()) << "a - b - format()";
    EXPECT_EQ("z-3", fc.simp().at(TP(1), 1).format()) << "a - b - format()";
}

// exception extender test
TYPED_TEST(FunctionalTest, TestExceptionExtender) {
    try {
        basic_function<TP> f ("{q, z} x-z");
        FAIL() << "No exception about parsing error";
    } catch (const cvmexception& ex) {
        EXPECT_EQ(CFUN_PARSEERROR, ex.cause()) << "CFUN_PARSEERROR exception cause";
    }
}

TYPED_TEST(FunctionalTest, TestZero) {
    basic_function<TP> rf0;
    EXPECT_STREQ("0", rf0.format().c_str()) << "rfunction default ctr";
    EXPECT_STREQ("0", rf0.simp().format().c_str()) << "rfunction default ctr - simp()";
    EXPECT_EQ(TP(0.), rf0()) << "rfunction default ctr - value";
    EXPECT_EQ(TP(0.), rf0.value(nullptr)) << "rfunction default ctr - value";
    EXPECT_EQ(TP(0.), rf0()) << "rfunction default ctr - value";
    EXPECT_EQ(TP(0.), rf0(1.1)) << "rfunction default ctr - value";
    EXPECT_EQ(TP(0.), rf0.drv()()) << "rfunction default ctr - drv - value";
    basic_function<TPC> cf0;
    EXPECT_STREQ("(0,0)", cf0.format().c_str()) << "cfunction default ctr";
    EXPECT_STREQ("(0,0)", cf0.simp().format().c_str()) << "cfunction default ctr - simp()";
    EXPECT_TRUE(TPC(0.,0.) == cf0()) << "cfunction default ctr - value";
    EXPECT_EQ(TPC(0.,0.), cf0.value(nullptr)) << "cfunction default ctr - value";
    EXPECT_EQ(TPC(0.,0.), cf0()) << "cfunction default ctr - value";
    EXPECT_EQ(TPC(0.,0.), cf0(TPC(TP(1.2), 3.4))) << "cfunction default ctr - value";
    EXPECT_EQ(TPC(0.,0.), cf0.drv()()) << "cfunction default ctr - drv - value";
}

// Fconst
TYPED_TEST(FunctionalTest, TestFconst) {
    basic_function<TP> rfc ("7.77 ");
    EXPECT_EQ(std::string("7.77"), rfc.format().substr(0, 4)) << "rfunction const - format()";
    EXPECT_EQ(std::string("7.77"), rfc.simp().format().substr(TP(0), 4)) << "rfunction const - simp() - format()";
    EXPECT_EQ(TP(7.77), rfc()) << "rfunction const - value";
    EXPECT_EQ(TP(7.77), rfc.value(nullptr)) << "rfunction const - value";
    EXPECT_EQ(TP(7.77), rfc()) << "rfunction const - value";
    EXPECT_EQ(TP(7.77), rfc(1.1)) << "rfunction const - value";
    EXPECT_EQ(TP(0.), rfc.drv()()) << "rfunction const - drv - value";

    basic_function<TPC> cfc ("(7.77, 8.88)");
    basic_function<TPC> cfc2 ("5.55");
    EXPECT_EQ(std::string("7.77"), cfc.format().substr(1, 4)) << "cfunction const - format()";
    EXPECT_EQ(std::string("8.88"), cfc.format().substr(cfc.format().find(",")+1, 4)) << "cfunction const - format()";
    EXPECT_EQ(std::string("7.77"), cfc.simp().format().substr(TP(1), 4)) << "cfunction const - simp() - format()";
    EXPECT_EQ(std::string("8.88"), cfc.simp().format().substr(cfc.format().find(",")+1, 4)) << "cfunction const - simp() - format()";
    EXPECT_EQ(TPC(7.77,8.88), cfc()) << "cfunction const - value";
    EXPECT_EQ(TPC(7.77,8.88), cfc.value(nullptr)) << "cfunction const - value";
    EXPECT_EQ(TPC(7.77,8.88), cfc()) << "cfunction const - value";
    EXPECT_EQ(TPC(7.77,8.88), cfc(1.23)) << "cfunction const - value";
    EXPECT_EQ(TPC(0.,0.), cfc.drv()()) << "cfunction const - drv - value";
    EXPECT_EQ(std::string("5.55"), cfc2.format().substr(TP(1), 4)) << "cfunction const - format()";
    EXPECT_EQ(std::string("0.00"), cfc2.format(2).substr(cfc2.format(2).find(",")+1, 4)) << "cfunction const - format()";
    EXPECT_EQ(std::string("5.55"), cfc2.simp().format().substr(TP(1), 4)) << "cfunction const - simp() - format()";
    EXPECT_EQ(std::string("0.00"), cfc2.simp().format(2).substr(cfc2.format(2).find(",")+1, 4)) << "cfunction const - simp() - format()";
    EXPECT_EQ(TPC(5.55,0.), cfc2()) << "cfunction const - value";
    EXPECT_EQ(TPC(5.55,0.), cfc2.value(nullptr)) << "cfunction const - value";
    EXPECT_EQ(TPC(5.55,0.), cfc2()) << "cfunction const - value";
    EXPECT_EQ(TPC(5.55,0.), cfc2(1.23)) << "cfunction const - value";
    EXPECT_EQ(TPC(0.,0.), cfc2.drv()()) << "cfunction const - drv - value";
}

// Finfinity
TYPED_TEST(FunctionalTest, TestFinfinity) {
    basic_function<TP> rfi("INF");
    EXPECT_EQ(std::string("(INF)"), rfi.format()) << "rfunction inf - format()";
    EXPECT_EQ(std::string("(INF)"), rfi.simp().format()) << "rfunction inf - simp() - format()";
    EXPECT_GT(rfi(), 1.e38) << "rfunction inf - value";
    EXPECT_GT(rfi.value(nullptr), 1.e38) << "rfunction inf - value";
    EXPECT_GT(rfi(), 1.e38) << "rfunction inf - value";
    EXPECT_GT(rfi(1.1), 1.e38) << "rfunction inf - value";
    EXPECT_EQ(std::string("(INF)"), rfi.drv().format()) << "rfunction inf - drv() - format()";

    basic_function<TP> rfmi("-INF");
    EXPECT_EQ(std::string("-(INF)"), rfmi.format()) << "rfunction minus inf - format()";
    EXPECT_EQ(std::string("(-INF)"), rfmi.simp().format()) << "rfunction minus inf - format()";
    EXPECT_LT(rfmi(), -1.e38) << "rfunction minus inf - value";
    EXPECT_LT(rfmi.value(nullptr), -1.e38) << "rfunction minus inf - value";
    EXPECT_LT(rfmi(), -1.e38) << "rfunction minus inf - value";
    EXPECT_LT(rfmi(1.1), -1.e38) << "rfunction minus inf - value";
}

// Fplus
TYPED_TEST(FunctionalTest, TestFplus) {
    basic_function<TP> rfplus("{x, y} x+y");
    EXPECT_EQ(std::string("x+y"), rfplus.format()) << "rfunction plus - format()";
    basic_function<TP> rfplus2("{x} x+x");
    EXPECT_EQ(std::string("2*x"), rfplus2.simp().format()) << "rfunction plus - simp() - format()";
    EXPECT_EQ(TP(14.72), rfplus2(7.36)) << "rfunction plus - value";
    {
        TP a[2];
        a[0] = 1.35;
        a[1] = 2.45;
        EXPECT_FLOAT_EQ(TP(3.8), rfplus.value(a)) << "rfunction plus - value";
        EXPECT_FLOAT_EQ(TP(3.8), rfplus(a)) << "rfunction plus - value";
    }
    EXPECT_EQ(std::string("2"), rfplus2.drv().format()) << "rfunction plus - drv() - format()";
    EXPECT_EQ(std::string("1"), rfplus.drv(0).format()) << "rfunction plus - drv() - format()";

    basic_function<TPC> cfplus("{x, y} x+y");
    EXPECT_EQ(std::string("x+y"), cfplus.format()) << "cfunction plus - format()";
    basic_function<TPC> cfplus2("{x} x+x");
    EXPECT_EQ(std::string("(2,0)*x"), cfplus2.simp().format()) << "cfunction plus - simp() - format()";
    EXPECT_EQ(TPC(14.72,6.34), cfplus2(TPC(7.36, 3.17))) << "cfunction plus - value";
    {
        TPC a[2];
        a[0] = TPC(7.31,3.18);
        a[1] = TPC(2.44,3.12);
        EXPECT_FLOAT_EQ(TP(9.75), cfplus.value(a).real()) << "cfunction plus - value";
        EXPECT_FLOAT_EQ(TP(6.3), cfplus.value(a).imag()) << "cfunction plus - value";
        EXPECT_FLOAT_EQ(TP(9.75), cfplus(a).real()) << "cfunction plus - value";
        EXPECT_FLOAT_EQ(TP(6.3), cfplus(a).imag()) << "cfunction plus - value";
    }
    EXPECT_EQ(std::string("(2,0)"), cfplus2.drv().format()) << "cfunction plus - drv() - format()";
    EXPECT_EQ(std::string("(1,0)"), cfplus.drv(0).format()) << "cfunction plus - drv() - format()";
}

// Fminus
TYPED_TEST(FunctionalTest, TestFminus) {
    basic_function<TP> rfminus("{x, y} x-y");
    EXPECT_EQ(std::string("x-y"), rfminus.format()) << "rfunction minus - format()";
    basic_function<TP> rfminus2("{x} x-x");
    EXPECT_EQ(std::string("0"), rfminus2.simp().format()) << "rfunction minus - simp() - format()";
    EXPECT_EQ(TP(0.), rfminus2(7.36)) << "rfunction minus - value";
    {
        TP a[2];
        a[0] = 1.45;
        a[1] = 2.35;
        EXPECT_FLOAT_EQ(TP(-0.9), rfminus.value(a)) << "rfunction minus - value";
    }
    EXPECT_EQ(std::string("0"), rfminus2.drv().format()) << "rfunction minus - drv() - format()";
    EXPECT_EQ(std::string("1"), rfminus.drv(0).format()) << "rfunction minus - drv() - format()";

    basic_function<TPC> cfminus("{x, y} x-y");
    EXPECT_EQ(std::string("x-y"), cfminus.format()) << "cfunction minus - format()";
    basic_function<TPC> cfminus2("{x} x-x");
    EXPECT_EQ(std::string("(0,0)"), cfminus2.simp().format()) << "cfunction minus - simp() - format()";
    EXPECT_EQ(TPC(0.,0.), cfminus2(TPC(TP(7.36), 3.17))) << "cfunction minus - value";
    {
        TPC a[2];
        a[0] = TPC(7.31,3.18);
        a[1] = TPC(2.44,2.12);
        EXPECT_FLOAT_EQ(TP(4.87), cfminus.value(a).real()) << "cfunction minus - value";
        EXPECT_FLOAT_EQ(TP(1.06), cfminus.value(a).imag()) << "cfunction minus - value";
    }
    EXPECT_EQ(std::string("(0,0)"), cfminus2.drv().format()) << "cfunction minus - drv() - format()";
    EXPECT_EQ(std::string("(1,0)"), cfminus.drv(0).format()) << "cfunction minus - drv() - format()";
}

// Fmult
TYPED_TEST(FunctionalTest, TestFmult) {
    basic_function<TP> rfmult("{x, y} x*y");
    EXPECT_EQ(std::string("x*y"), rfmult.format()) << "rfunction mult - format()";
    basic_function<TP> rfmult2("{x} x*x");
    EXPECT_EQ(std::string("x^2"), rfmult2.simp().format()) << "rfunction mult - simp() - format()";
    EXPECT_FLOAT_EQ(7.36*7.36, rfmult2(7.36)) << "rfunction mult - value";
    {
        TP a[2];
        a[0] = 1.45;
        a[1] = 2.35;
        EXPECT_FLOAT_EQ(1.45*2.35, rfmult.value(a)) << "rfunction mult - value";
    }
    EXPECT_EQ(std::string("2*x"), rfmult2.drv().format()) << "rfunction mult - drv() - format()";
    EXPECT_EQ(std::string("y"), rfmult.drv(0).format()) << "rfunction mult - drv() - format()";

    basic_function<TPC> cfmult("{x, y} x*y");
    EXPECT_EQ(std::string("x*y"), cfmult.format()) << "cfunction mult - format()";
    basic_function<TPC> cfmult2("{x} x*x");
    EXPECT_EQ(std::string("x^(2,0)"), cfmult2.simp().format()) << "cfunction mult - simp() - format()";
    TPC expected = TPC(7.36,3.17) * TPC(7.36, 3.17);
    EXPECT_FLOAT_EQ(expected.real(), cfmult2(TPC(7.36,3.17)).real()) << "cfunction mult - value";
    EXPECT_FLOAT_EQ(expected.imag(), cfmult2(TPC(7.36,3.17)).imag()) << "cfunction mult - value";
    {
        TPC a[2];
        a[0] = TPC(7.31,3.18);
        a[1] = TPC(2.44,3.12);
        TPC expected = TPC(7.31,3.18) * TPC(2.44,3.12);
        EXPECT_FLOAT_EQ(expected.real(), cfmult.value(a).real()) << "cfunction mult - value";
        EXPECT_FLOAT_EQ(expected.imag(), cfmult.value(a).imag()) << "cfunction mult - value";
    }
    EXPECT_EQ(std::string("(2,0)*x"), cfmult2.drv().format()) << "cfunction mult - drv() - format()";
    EXPECT_EQ(std::string("y"), cfmult.drv(0).format()) << "cfunction mult - drv() - format()";
}

// Fdiv
TYPED_TEST(FunctionalTest, TestFdiv) {
    basic_function<TP> rfdiv("{x, y} x/y");
    EXPECT_EQ(std::string("x/y"), rfdiv.format()) << "basic_function<TP> div - format()";
    basic_function<TP> rfdiv2("{x} x/x");
    EXPECT_EQ(std::string("1"), rfdiv2.simp().format()) << "basic_function<TP> div - simp() - format()";
    EXPECT_EQ(TP(1.), rfdiv2(7.36)) << "basic_function<TP> div - value";
    {
        TP a[2];
        a[0] = 1.45;
        a[1] = 2.35;
        EXPECT_FLOAT_EQ(1.45/2.35, rfdiv.value(a)) << "basic_function<TP> div - value";
    }
    EXPECT_EQ(std::string("0"), rfdiv2.drv().format()) << "basic_function<TP> div - drv() - format()";
    EXPECT_EQ(std::string("1/y"), rfdiv.drv(0).format()) << "basic_function<TP> div - drv() - format()";

    basic_function<TPC> cfdiv("{x, y} x/y");
    EXPECT_EQ(std::string("x/y"), cfdiv.format()) << "basic_function<TPC> div - format()";
    basic_function<TPC> cfdiv2("{x} x/x");
    EXPECT_EQ(std::string("(1,0)"), cfdiv2.simp().format()) << "basic_function<TPC> div - simp() - format()";
    EXPECT_EQ(TPC(1.,0.), cfdiv2(TPC(7.36, 3.17))) << "basic_function<TPC> div - value";
    {
        TPC a[2];
        a[0] = TPC(7.31,3.18);
        a[1] = TPC(2.44,3.12);
        TPC expected = TPC(7.31,3.18) / TPC(2.44,3.12);
        EXPECT_FLOAT_EQ(expected.real(), cfdiv.value(a).real()) << "basic_function<TPC> div - value";
        EXPECT_FLOAT_EQ(expected.imag(), cfdiv.value(a).imag()) << "basic_function<TPC> div - value";
    }
    EXPECT_EQ(std::string("(0,0)"), cfdiv2.drv().format()) << "basic_function<TPC> div - drv() - format()";
    EXPECT_EQ(std::string("(1,0)/y"), cfdiv.drv(0).format()) << "basic_function<TPC> div - drv() - format()";
}

// Fpower
TYPED_TEST(FunctionalTest, TestFpower) {
    basic_function<TP> rfpow("{x, y} x^y");
    EXPECT_EQ(std::string("x^y"), rfpow.format()) << "basic_function<TP> power - format()";
    basic_function<TP> rfpow2("{x} (x^3)^2");
    EXPECT_EQ(std::string("x^6"), rfpow2.simp().format()) << "basic_function<TP> power - simp() - format()";
    EXPECT_FLOAT_EQ(::pow(1.123, 6.), rfpow2(1.123)) << "basic_function<TP> power - value";
    {
        TP a[2];
        a[0] = 1.45;
        a[1] = 2.35;
        EXPECT_FLOAT_EQ(::pow(a[0], a[1]), rfpow.value(a)) << "basic_function<TP> power - value";
    }
    EXPECT_EQ(std::string("y*x^(y-1)"), rfpow.drv().format()) << "basic_function<TP> power - drv() - format()";
    EXPECT_EQ(std::string("x^y*log(x)"), rfpow.drv(1).format()) << "basic_function<TP> power - drv() - format()";
    EXPECT_EQ(std::string("6*x^5"), rfpow2.drv().format()) << "basic_function<TP> power - drv() - format()";

    basic_function<TPC> cfpow("{x, y} x^y");
    EXPECT_EQ(std::string("x^y"), cfpow.format()) << "basic_function<TPC> power - format()";
    basic_function<TPC> cfpow2("{x} (x^(3, 1))^(1, 2)");

    EXPECT_EQ(std::string("x^(1,7)"), cfpow2.simp().format()) << "basic_function<TPC> power - simp() - format()";
    TPC expected = ElementaryFunctions<TPC>::pow(TPC(7.36,3.17), TPC(1., 7.)); 

    EXPECT_FLOAT_EQ(expected.real(), cfpow2(TPC(7.36,3.17)).real()) << "basic_function<TPC> power - value";
    EXPECT_FLOAT_EQ(expected.imag(), cfpow2(TPC(7.36,3.17)).imag()) << "basic_function<TPC> power - value";
    {
        TPC a[2];
        a[0] = TPC(7.31,3.18);
        a[1] = TPC(2.44,3.12);
        expected = ElementaryFunctions<TPC>::pow(TPC(7.31,3.18), TPC(2.44,3.12));
        EXPECT_FLOAT_EQ(expected.real(), cfpow.value(a).real()) << "basic_function<TPC> power - value";
        EXPECT_FLOAT_EQ(expected.imag(), cfpow.value(a).imag()) << "basic_function<TPC> power - value";
    }
    EXPECT_EQ(std::string("y*x^(y-(1,-0))"), cfpow.drv().format()) << "basic_function<TPC> power - drv() - format()";
    EXPECT_EQ(std::string("x^y*log(x)"), cfpow.drv(1).format()) << "basic_function<TPC> power - drv() - format()";
    EXPECT_EQ(std::string("(1,7)*x^(0,7)"), cfpow2.drv().format()) << "basic_function<TPC> power - drv() - format()";
}

// Fsat
TYPED_TEST(FunctionalTest, TestFsat) {
    basic_function<TP> rfsat("{x, y} sat(x, y)");
    EXPECT_EQ(std::string("sat(x,y)"), rfsat.format()) << "basic_function<TP> sat - format()";
    basic_function<TP> rfsat2("{x} sat(x, 2+1)");
    EXPECT_EQ(std::string("sat(x,3)"), rfsat2.simp().format()) << "basic_function<TP> sat - simp() - format()";
    EXPECT_EQ(TP(-1.), rfsat2(-3.0001)) << "basic_function<TP> sat - value";
    EXPECT_EQ(TP(0.), rfsat2(-3.)) << "basic_function<TP> sat - value";
    EXPECT_EQ(TP(0.), rfsat2(3.)) << "basic_function<TP> sat - value";
    EXPECT_EQ(TP(1.), rfsat2(3.00001)) << "basic_function<TP> sat - value";
    {
        TP a[2];
        a[0] = 1.45;
        a[1] = 2.35;
        EXPECT_EQ(TP(0.), rfsat.value(a)) << "basic_function<TP> sat - value";
    }
    EXPECT_EQ(std::string("delta(x,3)+delta(x,(-3))"), rfsat2.drv().format()) << "basic_function<TP> sat - drv() - format()";

    basic_function<TPC> cfsat("{x, y} sat(x, y)");
    EXPECT_EQ(std::string("sat(x,y)"), cfsat.format()) << "basic_function<TPC> sat - format()";
    basic_function<TPC> cfsat2("{x} sat(x, (3, 1)+(1, 2))");
    EXPECT_EQ(std::string("sat(x,(4,3))"), cfsat2.simp().format()) << "basic_function<TPC> sat - simp() - format()";
    {
        TPC a[2];
        a[0] = TPC(7.31,3.18);
        a[1] = TPC(2.44,3.12);
        EXPECT_EQ(TPC(1.,0.), cfsat.value(a)) << "basic_function<TPC> sat - value";
    }
    EXPECT_EQ(std::string("delta(x,(4,3))+delta(x,(-4,-3))"), cfsat2.drv().format()) << "basic_function<TPC> sat - drv() - format()";
}

// Fexp
TYPED_TEST(FunctionalTest, TestFexp) {
    basic_function<TP> rfexp("{x} exp(x)");
    EXPECT_EQ(std::string("exp(x)"), rfexp.format()) << "basic_function<TP> exp - format()";
    EXPECT_FLOAT_EQ(cfun_e<TP>(), rfexp(1.)) << "basic_function<TP> exp - value";
    EXPECT_FLOAT_EQ(cfun_e<TP>() * cfun_e<TP>(), rfexp(2.)) << "basic_function<TP> exp - value";
    EXPECT_EQ(std::string("exp(x)"), rfexp.drv().format()) << "basic_function<TP> exp - drv() - format()";
    basic_function<TP> rfexp2("exp (0)");
    EXPECT_EQ(std::string("1"), rfexp2.simp().format()) << "basic_function<TP> exp - format()";

    basic_function<TPC> cfexp("{x} exp(x)");
    EXPECT_EQ(std::string("exp(x)"), cfexp.format()) << "basic_function<TPC> exp - format()";
    EXPECT_FLOAT_EQ(cfun_e<TP>(), cfexp(TPC(1.,0.)).real()) << "basic_function<TPC> exp - value";
    EXPECT_FLOAT_EQ(TP(0.), cfexp(TPC(1.,0.)).imag()) << "basic_function<TPC> exp - value";
    EXPECT_EQ(std::string("exp(x)"), cfexp.drv().format()) << "basic_function<TPC> exp - drv() - format()";
}

// Fsqrt
TYPED_TEST(FunctionalTest, TestFsqrt) {
    basic_function<TP> rfsqrt("{x} sqrt(x)");
    EXPECT_EQ(std::string("sqrt(x)"), rfsqrt.format()) << "basic_function<TP> sqrt - format()";
    EXPECT_FLOAT_EQ(::sqrt(2.), rfsqrt(2.)) << "basic_function<TP> sqrt - value";
    EXPECT_EQ(std::string("0.5/sqrt(x)"), rfsqrt.drv().format()) << "basic_function<TP> sqrt - drv() - format()";
    basic_function<TP> rfsqrt2("sqrt(4)");
    EXPECT_EQ(std::string("2"), rfsqrt2.simp().format()) << "basic_function<TP> sqrt - simp - format()";

    basic_function<TPC> cfsqrt("{x} sqrt(x)");
    EXPECT_EQ(std::string("sqrt(x)"), cfsqrt.format()) << "basic_function<TPC> sqrt - format()";
    EXPECT_EQ(TPC(0.,1.), cfsqrt(TPC(-1., 0.))) << "basic_function<TPC> sqrt - value";
    EXPECT_EQ(std::string("(0.5,0)/sqrt(x)"), cfsqrt.drv().format()) << "basic_function<TPC> sqrt - drv() - format()";
    basic_function<TPC> cfsqrt2("{x} sqrt(4, 0)");
    EXPECT_EQ(std::string("(2,0)"), cfsqrt2.simp().format()) << "basic_function<TPC> sqrt - simp() - format()";
}

// Flog
TYPED_TEST(FunctionalTest, TestFlog) {
    basic_function<TP> rflog("{x} log(x)");
    EXPECT_EQ(std::string("log(x)"), rflog.format()) << "basic_function<TP> log - format()";
    EXPECT_FLOAT_EQ(::log(2.), rflog(2.)) << "basic_function<TP> log - value";
    EXPECT_EQ(std::string("1/x"), rflog.drv().format()) << "basic_function<TP> log - drv() - format()";
    basic_function<TP> rflog2("{x} log(x^2)");
    EXPECT_EQ(std::string("log(x)*2"), rflog2.simp().format()) << "basic_function<TP> log - simp - format()";

    basic_function<TPC> cflog("{x} log(x)");
    EXPECT_EQ(std::string("log(x)"), cflog.format()) << "basic_function<TPC> log - format()";
    {
        std::ostringstream oss;
        oss.precision(15);
        oss << cflog(TPC(-1.,0.));
        EXPECT_EQ(std::string("(0,3.14159"), oss.str().substr(0, 10)) << "basic_function<TPC> log - value()";
    }
    basic_function<TPC> cflog2("{x} log(sqrt(x))");
    EXPECT_EQ(std::string("log(x)*(0.5,0)"), cflog2.simp().format()) << "basic_function<TPC> log - simp()";
}

// Flog10
TYPED_TEST(FunctionalTest, TestFlog10) {
    basic_function<TP> rflog10("{x} log10(x)");
    EXPECT_EQ(std::string("log10(x)"), rflog10.format()) << "basic_function<TP> log10 - format()";
    EXPECT_FLOAT_EQ(::log10(2.), rflog10(2.)) << "basic_function<TP> log10 - value";
    EXPECT_EQ(std::string("0.434294/x"), rflog10.drv().format()) << "basic_function<TP> log10 - drv - format()";
    basic_function<TP> rflog10_2("{x} log10(x^2)");
    EXPECT_EQ(std::string("log10(x)*2"), rflog10_2.simp().format()) << "basic_function<TP> log10 - simp - format()";

    basic_function<TPC> cflog10("{x} log10(x)");
    EXPECT_EQ(std::string("log10(x)"), cflog10.format()) << "basic_function<TPC> log10 - format()";
    {
        std::ostringstream oss;
        oss.precision(15);
        oss << cflog10(TPC(-1.,0.));
        EXPECT_EQ(std::string("(0,1.36437"), oss.str().substr(0, 10)) << "basic_function<TPC> log10 - value()";
    }
    basic_function<TPC> cflog10_2("{x} log10(sqrt(x))");
    EXPECT_EQ(std::string("log10(x)*(0.5,0)"), cflog10_2.simp().format()) << "basic_function<TPC> log10 - simp()";
}

// Fsin
TYPED_TEST(FunctionalTest, TestFsin) {
    basic_function<TP> rfsin("{x} sin(x)");
    EXPECT_EQ(std::string("sin(x)"), rfsin.format()) << "basic_function<TP> sin - format()";
    EXPECT_FLOAT_EQ(::sin(2.), rfsin(2.)) << "basic_function<TP> sin - value";
    EXPECT_EQ(std::string("cos(x)"), rfsin.drv().format()) << "basic_function<TP> sin - drv - format()";
    basic_function<TP> rfsin_2("sin(1)");
    EXPECT_EQ(std::string("0.841471"), rfsin_2.simp().format()) << "basic_function<TP> sin - simp - format()";

    basic_function<TPC> cfsin("{x} sin(x)");
    EXPECT_EQ(std::string("sin(x)"), cfsin.format()) << "basic_function<TPC> sin - format()";
    {
        std::ostringstream oss;
        oss.precision(15);
        oss << cfsin(TPC(0.,1.));
        EXPECT_EQ(std::string("(0,1.17520"), oss.str().substr(0, 10)) << "basic_function<TPC> sin - value()";
    }
    basic_function<TPC> cfsin_2("{x} sin(0, 0)");
    EXPECT_EQ(std::string("(0,0)"), cfsin_2.simp().format()) << "basic_function<TPC> sin - simp()";
}

// Fcos
TYPED_TEST(FunctionalTest, TestFcos) {
    basic_function<TP> rfcos("{x} cos(x)");
    EXPECT_EQ(std::string("cos(x)"), rfcos.format()) << "basic_function<TP> cos - format()";
    EXPECT_FLOAT_EQ(::cos(2.), rfcos(2.)) << "basic_function<TP> cos - value";
    EXPECT_EQ(std::string("-sin(x)"), rfcos.drv().format()) << "basic_function<TP> cos - drv - format()";
    basic_function<TP> rfcos_2("cos(1)");
    EXPECT_EQ(std::string("0.540302"), rfcos_2.simp().format()) << "basic_function<TP> cos - simp - format()";

    basic_function<TPC> cfcos("{x} cos(x)");
    EXPECT_EQ(std::string("cos(x)"), cfcos.format()) << "basic_function<TPC> cos - format()";
    {
        std::ostringstream oss;
        oss.precision(15);
        oss << cfcos(TPC(-1.,1.));
        EXPECT_EQ(std::string("(0.8337"), oss.str().substr(0, 7)) << "basic_function<TPC> cos - value()";
        EXPECT_EQ(std::string("0.988897"), oss.str().substr(oss.str().find(",")+1, 8)) << "basic_function<TPC> cos - value()";
    }
    basic_function<TPC> cfcos_2("{x} cos(1, -1)");
    EXPECT_EQ(std::string("(0.83373,0.988898)"), cfcos_2.simp().format()) << "basic_function<TPC> cos - simp()";
}

// Ftan
TYPED_TEST(FunctionalTest, TestFtan) {
    basic_function<TP> rftan("{x} tan(x)");
    EXPECT_EQ(std::string("tan(x)"), rftan.format()) << "basic_function<TP> tan - format()";
    EXPECT_FLOAT_EQ(::tan(2.), rftan(2.)) << "basic_function<TP> tan - value";
    EXPECT_EQ(std::string("1/cos(x)^2"), rftan.drv().format()) << "basic_function<TP> tan - drv - format()";
    basic_function<TP> rftan_2("tan(1)");
    EXPECT_EQ(std::string("1.55741"), rftan_2.simp().format()) << "basic_function<TP> tan - simp - format()";

    basic_function<TPC> cftan("{x} tan(x)");
    EXPECT_EQ(std::string("tan(x)"), cftan.format()) << "basic_function<TPC> tan - format()";
    {
        std::ostringstream oss;
        oss.precision(15);
        oss << cftan(TPC(-1.,1.));
        EXPECT_EQ(std::string("(-0.27175"), oss.str().substr(0, 9)) << "basic_function<TPC> tan - value()";
        EXPECT_EQ(std::string("1.083923"), oss.str().substr(oss.str().find(",")+1, 8)) << "basic_function<TPC> tan - value()";
    }
    basic_function<TPC> cftan_2("{x} tan(0, 0)");
    EXPECT_EQ("(0,0)", cftan_2.simp().format()) << "basic_function<TPC> tan - simp()";
}

// Fasin
TYPED_TEST(FunctionalTest, TestFasin) {
    basic_function<TP> rfasin("{x} asin(x)");
    EXPECT_EQ("asin(x)", rfasin.format()) << "basic_function<TP> asin - format()";
    EXPECT_FLOAT_EQ(::asin(0.5), rfasin(0.5)) << "basic_function<TP> asin - value";
    EXPECT_EQ("1/sqrt(1-x^2)", rfasin.drv().format()) << "basic_function<TP> asin - drv - format()";
    basic_function<TP> rfasin_2("asin(1)");
    EXPECT_EQ("1.5708", rfasin_2.simp().format()) << "basic_function<TP> asin - simp - format()";

    basic_function<TPC> cfasin("{x} asin(x)");
    EXPECT_EQ("asin(x)", cfasin.format()) << "basic_function<TPC> asin - format()";
    {
        std::ostringstream oss;
        oss.precision(15);
        oss << cfasin(TPC(-1.,1.));
        EXPECT_EQ("(-0.66623", oss.str().substr(0, 9)) << "basic_function<TPC> asin - value()";
        EXPECT_EQ("1.061275", oss.str().substr(oss.str().find(",")+1, 8)) << "basic_function<TPC> asin - value()";
    }
    basic_function<TPC> cfasin_2("{x} asin(0, 0)");
    EXPECT_EQ("(0,0)", cfasin_2.simp().format()) << "basic_function<TPC> asin - simp()";
}

