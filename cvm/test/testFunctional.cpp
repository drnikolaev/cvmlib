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
    EXPECT_EQ("sign(x+2)", fc.simp().at(0, 0).format()) << "a - b - format()";
    EXPECT_EQ("z-3", fc.simp().at(1, 1).format()) << "a - b - format()";
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
    EXPECT_EQ(std::string("7.77"), rfc.simp().format().substr(0, 4)) << "rfunction const - simp() - format()";
    EXPECT_EQ(TP(7.77), rfc()) << "rfunction const - value";
    EXPECT_EQ(TP(7.77), rfc.value(nullptr)) << "rfunction const - value";
    EXPECT_EQ(TP(7.77), rfc()) << "rfunction const - value";
    EXPECT_EQ(TP(7.77), rfc(1.1)) << "rfunction const - value";
    EXPECT_EQ(TP(0.), rfc.drv()()) << "rfunction const - drv - value";

    basic_function<TPC> cfc ("(7.77, 8.88)");
    basic_function<TPC> cfc2 ("5.55");
    EXPECT_EQ(std::string("7.77"), cfc.format().substr(1, 4)) << "cfunction const - format()";
    EXPECT_EQ(std::string("8.88"), cfc.format().substr(cfc.format().find(",")+1, 4)) << "cfunction const - format()";
    EXPECT_EQ(std::string("7.77"), cfc.simp().format().substr(1, 4)) << "cfunction const - simp() - format()";
    EXPECT_EQ(std::string("8.88"), cfc.simp().format().substr(cfc.format().find(",")+1, 4)) << "cfunction const - simp() - format()";
    EXPECT_EQ(TPC(7.77,8.88), cfc()) << "cfunction const - value";
    EXPECT_EQ(TPC(7.77,8.88), cfc.value(nullptr)) << "cfunction const - value";
    EXPECT_EQ(TPC(7.77,8.88), cfc()) << "cfunction const - value";
    EXPECT_EQ(TPC(7.77,8.88), cfc(1.23)) << "cfunction const - value";
    EXPECT_EQ(TPC(0.,0.), cfc.drv()()) << "cfunction const - drv - value";
    EXPECT_EQ(std::string("5.55"), cfc2.format().substr(1, 4)) << "cfunction const - format()";
    EXPECT_EQ(std::string("0.00"), cfc2.format(2).substr(cfc2.format(2).find(",")+1, 4)) << "cfunction const - format()";
    EXPECT_EQ(std::string("5.55"), cfc2.simp().format().substr(1, 4)) << "cfunction const - simp() - format()";
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
        EXPECT_FLOAT_EQ(3.8F, static_cast<float>(rfplus.value(a))) << "rfunction plus - value";
        EXPECT_FLOAT_EQ(3.8F, static_cast<float>(rfplus(a))) << "rfunction plus - value";
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
        EXPECT_FLOAT_EQ(9.75F, static_cast<float>(cfplus.value(a).real())) << "cfunction plus - value";
        EXPECT_FLOAT_EQ(6.3F, static_cast<float>(cfplus.value(a).imag())) << "cfunction plus - value";
        EXPECT_FLOAT_EQ(9.75F, static_cast<float>(cfplus(a).real())) << "cfunction plus - value";
        EXPECT_FLOAT_EQ(6.3F, static_cast<float>(cfplus(a).imag())) << "cfunction plus - value";
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
        EXPECT_FLOAT_EQ(-0.9F, static_cast<float>(rfminus.value(a))) << "rfunction minus - value";
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
        EXPECT_FLOAT_EQ(4.87F, static_cast<float>(cfminus.value(a).real())) << "cfunction minus - value";
        EXPECT_FLOAT_EQ(1.06F, static_cast<float>(cfminus.value(a).imag())) << "cfunction minus - value";
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
    EXPECT_FLOAT_EQ(7.36F*7.36F, static_cast<float>(rfmult2(7.36))) << "rfunction mult - value";
    {
        TP a[2];
        a[0] = 1.45;
        a[1] = 2.35;
        EXPECT_FLOAT_EQ(1.45F*2.35F, static_cast<float>(rfmult.value(a))) << "rfunction mult - value";
    }
    EXPECT_EQ(std::string("2*x"), rfmult2.drv().format()) << "rfunction mult - drv() - format()";
    EXPECT_EQ(std::string("y"), rfmult.drv(0).format()) << "rfunction mult - drv() - format()";

    basic_function<TPC> cfmult("{x, y} x*y");
    EXPECT_EQ(std::string("x*y"), cfmult.format()) << "cfunction mult - format()";
    basic_function<TPC> cfmult2("{x} x*x");
    EXPECT_EQ(std::string("x^(2,0)"), cfmult2.simp().format()) << "cfunction mult - simp() - format()";
    std::complex<float> expected = std::complex<float>(7.36,3.17) * std::complex<float>(7.36, 3.17);
    EXPECT_FLOAT_EQ(expected.real(), static_cast<float>(cfmult2(TPC(7.36,3.17)).real())) << "cfunction mult - value";
    EXPECT_FLOAT_EQ(expected.imag(), static_cast<float>(cfmult2(TPC(7.36,3.17)).imag())) << "cfunction mult - value";
    {
        TPC a[2];
        a[0] = TPC(7.31,3.18);
        a[1] = TPC(2.44,3.12);
        TPC expected = TPC(7.31,3.18) * TPC(2.44,3.12);
        EXPECT_DOUBLE_EQ(expected.real(), cfmult.value(a).real()) << "cfunction mult - value";
        EXPECT_DOUBLE_EQ(expected.imag(), cfmult.value(a).imag()) << "cfunction mult - value";
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
        EXPECT_FLOAT_EQ(1.45F/2.35F, static_cast<float>(rfdiv.value(a))) << "basic_function<TP> div - value";
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
        EXPECT_DOUBLE_EQ(expected.real(), cfdiv.value(a).real()) << "basic_function<TPC> div - value";
        EXPECT_DOUBLE_EQ(expected.imag(), cfdiv.value(a).imag()) << "basic_function<TPC> div - value";
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
    EXPECT_FLOAT_EQ(static_cast<float>(::pow(1.123, 6.)), static_cast<float>(rfpow2(1.123))) << "basic_function<TP> power - value";
    {
        TP a[2];
        a[0] = 1.45;
        a[1] = 2.35;
        EXPECT_FLOAT_EQ(static_cast<float>(::pow(a[0], a[1])), static_cast<float>(rfpow.value(a))) << "basic_function<TP> power - value";
    }
    EXPECT_EQ(std::string("y*x^(y-1)"), rfpow.drv().format()) << "basic_function<TP> power - drv() - format()";
    EXPECT_EQ(std::string("x^y*log(x)"), rfpow.drv(1).format()) << "basic_function<TP> power - drv() - format()";
    EXPECT_EQ(std::string("6*x^5"), rfpow2.drv().format()) << "basic_function<TP> power - drv() - format()";

    basic_function<TPC> cfpow("{x, y} x^y");
    EXPECT_EQ(std::string("x^y"), cfpow.format()) << "basic_function<TPC> power - format()";
    basic_function<TPC> cfpow2("{x} (x^(3, 1))^(1, 2)");

    EXPECT_EQ(std::string("x^(1,7)"), cfpow2.simp().format()) << "basic_function<TPC> power - simp() - format()";
    TPC expected = ElementaryFunctions<TPC>::pow(TPC(7.36,3.17), TPC(1., 7.)); 

    EXPECT_DOUBLE_EQ(expected.real(), cfpow2(TPC(7.36,3.17)).real()) << "basic_function<TPC> power - value";
    EXPECT_DOUBLE_EQ(expected.imag(), cfpow2(TPC(7.36,3.17)).imag()) << "basic_function<TPC> power - value";
    {
        TPC a[2];
        a[0] = TPC(7.31,3.18);
        a[1] = TPC(2.44,3.12);
        expected = ElementaryFunctions<TPC>::pow(TPC(7.31,3.18), TPC(2.44,3.12));
        EXPECT_DOUBLE_EQ(expected.real(), cfpow.value(a).real()) << "basic_function<TPC> power - value";
        EXPECT_DOUBLE_EQ(expected.imag(), cfpow.value(a).imag()) << "basic_function<TPC> power - value";
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
    EXPECT_DOUBLE_EQ(cfun_e<TP>(), rfexp(1.)) << "basic_function<TP> exp - value";
    EXPECT_FLOAT_EQ(cfun_e<float>() * cfun_e<float>(), static_cast<float>(rfexp(2.))) << "basic_function<TP> exp - value";
    EXPECT_EQ(std::string("exp(x)"), rfexp.drv().format()) << "basic_function<TP> exp - drv() - format()";
    basic_function<TP> rfexp2("exp (0)");
    EXPECT_EQ(std::string("1"), rfexp2.simp().format()) << "basic_function<TP> exp - format()";

    basic_function<TPC> cfexp("{x} exp(x)");
    EXPECT_EQ(std::string("exp(x)"), cfexp.format()) << "basic_function<TPC> exp - format()";
    EXPECT_FLOAT_EQ(cfun_e<float>(), static_cast<float>(cfexp(TPC(1.,0.)).real())) << "basic_function<TPC> exp - value";
    EXPECT_DOUBLE_EQ(TP(0.), cfexp(TPC(1.,0.)).imag()) << "basic_function<TPC> exp - value";
    EXPECT_EQ(std::string("exp(x)"), cfexp.drv().format()) << "basic_function<TPC> exp - drv() - format()";
}

// Fsqrt
TYPED_TEST(FunctionalTest, TestFsqrt) {
    basic_function<TP> rfsqrt("{x} sqrt(x)");
    EXPECT_EQ(std::string("sqrt(x)"), rfsqrt.format()) << "basic_function<TP> sqrt - format()";
    EXPECT_FLOAT_EQ(static_cast<float>(::sqrt(2.)), static_cast<float>(rfsqrt(2.))) << "basic_function<TP> sqrt - value";
    EXPECT_EQ(std::string("0.5/sqrt(x)"), rfsqrt.drv().format()) << "basic_function<TP> sqrt - drv() - format()";
    basic_function<TP> rfsqrt2("sqrt(4)");
    EXPECT_EQ(std::string("2"), rfsqrt2.simp().format()) << "basic_function<TP> sqrt - simp - format()";

    basic_function<TPC> cfsqrt("{x} sqrt(x)");
    EXPECT_EQ(std::string("sqrt(x)"), cfsqrt.format()) << "basic_function<TPC> sqrt - format()";
    EXPECT_NEAR(TP(0.), cfsqrt(TPC(-1., 0.)).real(), s<TP>()) << "basic_function<TPC> sqrt - value";
    EXPECT_NEAR(TP(1.), cfsqrt(TPC(-1., 0.)).imag(), s<TP>()) << "basic_function<TPC> sqrt - value";
    EXPECT_EQ(std::string("(0.5,0)/sqrt(x)"), cfsqrt.drv().format()) << "basic_function<TPC> sqrt - drv() - format()";
    basic_function<TPC> cfsqrt2("{x} sqrt(4, 0)");
    EXPECT_EQ(std::string("(2,0)"), cfsqrt2.simp().format()) << "basic_function<TPC> sqrt - simp() - format()";
}

// Flog
TYPED_TEST(FunctionalTest, TestFlog) {
    basic_function<TP> rflog("{x} log(x)");
    EXPECT_EQ(std::string("log(x)"), rflog.format()) << "basic_function<TP> log - format()";
    EXPECT_FLOAT_EQ(static_cast<float>(::log(2.)), static_cast<float>(rflog(2.))) << "basic_function<TP> log - value";
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
    EXPECT_FLOAT_EQ(static_cast<float>(::log10(2.)), static_cast<float>(rflog10(2.))) << "basic_function<TP> log10 - value";
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
    EXPECT_FLOAT_EQ(static_cast<float>(::sin(2.)), static_cast<float>(rfsin(2.))) << "basic_function<TP> sin - value";
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
    EXPECT_FLOAT_EQ(static_cast<float>(::cos(2.)), static_cast<float>(rfcos(2.))) << "basic_function<TP> cos - value";
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
    EXPECT_FLOAT_EQ(static_cast<float>(::tan(2.)), static_cast<float>(rftan(2.))) << "basic_function<TP> tan - value";
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
    EXPECT_FLOAT_EQ(static_cast<float>(::asin(0.5)), static_cast<float>(rfasin(0.5))) << "basic_function<TP> asin - value";
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

// Facos
TYPED_TEST(FunctionalTest, TestFacos) {
    basic_function<TP> rfacos("{x} acos(x)");
    EXPECT_EQ("acos(x)", rfacos.format()) << "basic_function<TP> acos - format()";
    EXPECT_FLOAT_EQ(static_cast<float>(::acos(0.5)), static_cast<float>(rfacos(0.5))) << "basic_function<TP> acos - value";
    EXPECT_EQ("(-1)/sqrt(1-x^2)", rfacos.drv().format()) << "basic_function<TP> acos - drv - format()";
    basic_function<TP> rfacos_2("acos(1)");
    EXPECT_EQ("0", rfacos_2.simp().format()) << "basic_function<TP> acos - simp - format()";

    basic_function<TPC> cfacos("{x} acos(x)");
    EXPECT_EQ("acos(x)", cfacos.format()) << "basic_function<TPC> acos - format()";
    {
        std::ostringstream oss;
        oss.precision(15);
        oss << cfacos(TPC(-1.,1.));
        EXPECT_EQ("(2.237035", oss.str().substr(0, 9)) << "basic_function<TPC> acos - value()";
        EXPECT_EQ("-1.061275", oss.str().substr(oss.str().find(",")+1, 9)) << "basic_function<TPC> acos - value()";
    }
    basic_function<TPC> cfacos_2("{x} acos(0, 0)");
    EXPECT_EQ("(1.5708,0)", cfacos_2.simp().format()) << "basic_function<TPC> acos - simp()";
}

// Fatan
TYPED_TEST(FunctionalTest, TestFatan) {
    basic_function<TP> rfatan("{x} atan(x)");
    EXPECT_EQ("atan(x)", rfatan.format()) << "basic_function<TP> atan - format()";
    EXPECT_FLOAT_EQ(static_cast<float>(::atan(0.5)), static_cast<float>(rfatan(0.5))) << "basic_function<TP> atan - value";
    EXPECT_EQ("1/(1+x^2)", rfatan.drv().format()) << "basic_function<TP> atan - drv - format()";
    basic_function<TP> rfatan_2("atan(1)");
    EXPECT_EQ("0.785398", rfatan_2.simp().format()) << "basic_function<TP> atan - simp - format()";

    basic_function<TPC> cfatan("{x} atan(x)");
    EXPECT_EQ("atan(x)", cfatan.format()) << "basic_function<TPC> atan - format()";
    {
        std::ostringstream oss;
        oss.precision(15);
        oss << cfatan(TPC(-1.,1.));
        EXPECT_EQ("(-1.01722", oss.str().substr(0, 9)) << "basic_function<TPC> acos - value()";
        EXPECT_EQ("0.402359", oss.str().substr(oss.str().find(",")+1, 8)) << "basic_function<TPC> acos - value()";
    }
    basic_function<TPC> cfatan_2("{x} atan(0, 0)");
    EXPECT_EQ("(0,0)", cfatan_2.simp().format()) << "basic_function<TPC> atan - simp()";
}

// Fsinh
TYPED_TEST(FunctionalTest, TestFsinh) {
    basic_function<TP> rfsinh("{x} sinh(x)");
    EXPECT_EQ("sinh(x)", rfsinh.format()) << "basic_function<TP> sinh - format()";
    EXPECT_FLOAT_EQ(static_cast<float>(1.1752011936438014), static_cast<float>(rfsinh(1.))) << "basic_function<TP> sinh - value";
    EXPECT_EQ("cosh(x)", rfsinh.drv().format()) << "basic_function<TP> sinh - drv - format()";
    basic_function<TP> rfsinh2("sinh(-1.)");
    EXPECT_EQ("(-1.1752)", rfsinh2.simp().format()) << "basic_function<TP> sinh - simp - format()";

    basic_function<TPC> cfsinh("{x} sinh(x)");
    EXPECT_EQ("sinh(x)", cfsinh.format()) << "basic_function<TPC> sinh - format()";

    EXPECT_FLOAT_EQ(static_cast<float>(-0.63496381), static_cast<float>(cfsinh(TPC(-1.,1.)).real())) << "basic_function<TPC> sinh - value";
    EXPECT_FLOAT_EQ(static_cast<float>(1.2984574), static_cast<float>(cfsinh(TPC(-1.,1.)).imag())) << "basic_function<TPC> sinh - value";
    basic_function<TPC> cfsinh2("sinh(0, 0)");
    EXPECT_EQ("(0,0)", cfsinh2.simp().format()) << "basic_function<TPC> sinh - simp - format()";
}

// Fcosh
TYPED_TEST(FunctionalTest, TestFcosh) {
    basic_function<TP> rfcosh("{x} cosh(x)");
    EXPECT_EQ("cosh(x)", rfcosh.format()) << "basic_function<TP> cosh - format()";
    EXPECT_FLOAT_EQ(static_cast<float>(1.5430806348152437), static_cast<float>(rfcosh(1.))) << "basic_function<TP> cosh - value";
    EXPECT_EQ("-sinh(x)", rfcosh.drv().format()) << "basic_function<TP> cosh - drv - format()";
    basic_function<TP> rfcosh2("cosh(-1.)");
    EXPECT_EQ("1.54308", rfcosh2.simp().format()) << "basic_function<TP> sinh - simp - format()";

    basic_function<TPC> cfcosh("{x} cosh(x)");
    EXPECT_EQ("cosh(x)", cfcosh.format()) << "basic_function<TPC> cosh - format()";
    
    EXPECT_FLOAT_EQ(static_cast<float>(0.83373002513114913), cfcosh(TPC(-1.,1.)).real()) << "basic_function<TPC> cosh - value";
    EXPECT_FLOAT_EQ(static_cast<float>(-0.98889770576286506), cfcosh(TPC(-1.,1.)).imag()) << "basic_function<TPC> cosh - value";

    basic_function<TPC> cfcosh2("cosh(0, 0)");
    EXPECT_EQ("(1,0)", cfcosh2.simp().format()) << "basic_function<TPC> cosh - simp - format()";
}

// Ftanh
TYPED_TEST(FunctionalTest, TestFtanh) {
    basic_function<TP> rftanh("{x} tanh(x)");
    EXPECT_EQ("tanh(x)", rftanh.format()) << "basic_function<TP> tanh - format()";
    EXPECT_FLOAT_EQ(static_cast<float>(::tanh(-1.)), static_cast<float>(rftanh(-1.))) << "basic_function<TP> tanh - value";
    EXPECT_EQ("1/cosh(x)^2", rftanh.drv().format()) << "basic_function<TP> tanh - drv - format()";
    basic_function<TP> rftanh2("{x} tanh(x^2)");
    EXPECT_EQ("2*x/cosh(x^2)^2", rftanh2.drv().format()) << "basic_function<TP> tanh - drv - format()";

    basic_function<TPC> cftanh("{x} tanh(x)");
    EXPECT_EQ("tanh(x)", cftanh.format()) << "basic_function<TPC> tanh - format()";
    EXPECT_FLOAT_EQ(static_cast<float>(std::tanh(TPC(-1.,1.)).real()),
                    static_cast<float>(cftanh(TPC(-1.,1.)).real())) << "basic_function<TPC> tanh - value";
    EXPECT_FLOAT_EQ(static_cast<float>(std::tanh(TPC(-1.,1.)).imag()),
                    static_cast<float>(cftanh(TPC(-1.,1.)).imag())) << "basic_function<TPC> tanh - value";
    basic_function<TPC> cftanh2("tanh(0, 0)");
    EXPECT_EQ("(0,0)", cftanh2.simp().format()) << "basic_function<TPC> tanh - simp - format()";
}

// Fuminus
TYPED_TEST(FunctionalTest, TestFuminus) {
    basic_function<TP> rfuminus("{x} -x");
    EXPECT_EQ("-x", rfuminus.format()) << "basic_function<TP> unary minus - format()";
    EXPECT_EQ(TP(-0.5), rfuminus(0.5)) << "basic_function<TP> unary minus - value";
    EXPECT_EQ("(-1)", rfuminus.drv().format()) << "basic_function<TP> unary minus - drv - format()";
    basic_function<TP> rfuminus_2("{x} -(-x)");
    EXPECT_EQ("x", rfuminus_2.simp().format()) << "basic_function<TP> unary minus - simp - format()";

    basic_function<TPC> cfuminus("{x} -x");
    EXPECT_EQ("-x", cfuminus.format()) << "basic_function<TPC> unary minus - format()";
    {
        std::ostringstream oss;
        oss.precision(15);
        oss << cfuminus(TPC(-1.,1.));
        EXPECT_EQ("(1,-1)", oss.str()) << "basic_function<TPC> unary minus - value()";
        }
    basic_function<TPC> cfuminus_2("{x} -(1, 2)");
    EXPECT_EQ("(-1,-2)", cfuminus_2.simp().format()) << "basic_function<TPC> unary minus - simp()";
}

// Fsign
TYPED_TEST(FunctionalTest, TestFsign) {
    basic_function<TP> rfsign("{x} sign(x)");
    EXPECT_EQ("sign(x)", rfsign.format()) << "basic_function<TP> sign - format()";
    EXPECT_EQ(TP(1.), rfsign(0.5)) << "basic_function<TP> sign - value";
    EXPECT_EQ(TP(0.), rfsign(0.)) << "basic_function<TP> sign - value";
    EXPECT_EQ(TP(-1.), rfsign(-0.01)) << "basic_function<TP> sign - value";
    EXPECT_EQ("delta(x,0)", rfsign.drv().format()) << "basic_function<TP> sign - drv - format()";

    basic_function<TPC> cfsign("{x} sign(x)");
    EXPECT_EQ("sign(x)", cfsign.format()) << "basic_function<TPC> sign - format()";
    EXPECT_EQ(TPC(-1.,0.), cfsign(TPC(-1., 1.))) << "basic_function<TPC> sign - value";
    EXPECT_EQ(TPC(0.,0.), cfsign(TPC(0., 1.))) << "basic_function<TPC> sign - value";
    EXPECT_EQ(TPC(1.,0.), cfsign(TPC(0.5, -1.))) << "basic_function<TPC> sign - value";
    EXPECT_EQ("delta(x,(0,0))", cfsign.drv().format()) << "basic_function<TPC> sign - drv - format()";
}

// Fabs
TYPED_TEST(FunctionalTest, TestFabs) {
    basic_function<TP> rfabs("{x} abs(x)");
    EXPECT_EQ("abs(x)", rfabs.format()) << "basic_function<TP> abs - format()";
    EXPECT_EQ(TP(0.5), rfabs(-0.5)) << "basic_function<TP> abs - value";
    EXPECT_EQ("sign(x)", rfabs.drv().format()) << "basic_function<TP> abs - drv - format()";

    basic_function<TPC> cfabs("{x} abs(x)");
    EXPECT_EQ("abs(x)", cfabs.format()) << "basic_function<TPC> abs - format()";
    EXPECT_FLOAT_EQ(static_cast<float>(::sqrt(2.)),
                    static_cast<float>(cfabs(TPC(-1.,1.)).real())) << "basic_function<TPC> abs - value";
    EXPECT_FLOAT_EQ(0.F,
                    static_cast<float>(cfabs(TPC(-1.,1.)).imag())) << "basic_function<TPC> abs - value";
    EXPECT_EQ("sign(x)", cfabs.drv().format()) << "basic_function<TPC> abs - drv - format()";
}

// Fdelta
TYPED_TEST(FunctionalTest, TestFdelta) {
    basic_function<TP> rfdelta("{x} delta(x, 1.)");
    EXPECT_EQ("delta(x,1)", rfdelta.format()) << "basic_function<TP> delta - format()";
    EXPECT_EQ(TP(0.), rfdelta(-0.5)) << "basic_function<TP> delta - value";
    EXPECT_EQ((std::numeric_limits<TP>::max)(), rfdelta(1.)) << "basic_function<TP> delta - value";
    EXPECT_EQ("delta(x,1)", rfdelta.drv().format()) << "basic_function<TP> delta - drv - format()";

    basic_function<TPC> cfdelta("{x} delta(x, 1)");
    EXPECT_EQ("delta(x,(1,0))", cfdelta.format()) << "basic_function<TPC> delta - format()";

    EXPECT_FLOAT_EQ(0.F, static_cast<float>(cfdelta(TPC(-1.,1.)).real())) << "basic_function<TPC> abs - value";
    EXPECT_FLOAT_EQ(0.F, static_cast<float>(cfdelta(TPC(-1.,1.)).imag())) << "basic_function<TPC> abs - value";

    EXPECT_EQ("delta(x,(1,0))", cfdelta.drv().format()) << "basic_function<TPC> delta - drv - format()";
}

// Fiif
TYPED_TEST(FunctionalTest, TestFiif) {
    basic_function<TP> rfiif("{x} iif(x+1, 1., 2.)");
    EXPECT_EQ("iif(x+1,1,2)", rfiif.format()) << "basic_function<TP> iif - format()";
    EXPECT_EQ(TP(1.), rfiif(-1.5)) << "basic_function<TP> iif - value";
    EXPECT_EQ(TP(2.), rfiif(-1.)) << "basic_function<TP> iif - value";
    EXPECT_EQ("iif(x+1,0,0)", rfiif.drv().format()) << "basic_function<TP> iif - drv - format()";

    basic_function<TPC> cfiif("{x} iif(x+(1., 1.), (1., 1.), (2., 2.))");
    EXPECT_EQ("iif(x+(1,1),(1,1),(2,2))", cfiif.format()) << "basic_function<TPC> iif - format()";

    EXPECT_EQ(TPC(2., 2.), cfiif(TPC(-1.,1.))) << "basic_function<TPC> iif - value";
    EXPECT_EQ("iif(x+(1,1),(0,0),(0,0))", cfiif.drv().format()) << "basic_function<TPC> iif - drv - format()";
}

// Fsinint
TYPED_TEST(FunctionalTest, TestFsinint) {
    basic_function<TP> rfSi("{x} sinint(x)");
    EXPECT_EQ("sinint(x)", rfSi.format()) << "basic_function<TP> sinint - format()";
    EXPECT_FLOAT_EQ(0.493107418043067F, static_cast<float>(rfSi(0.5))) << "basic_function<TP> sinint - value";
    EXPECT_EQ("sin(x)/x", rfSi.drv().format()) << "basic_function<TP> sinint - drv - format()";

    basic_function<TPC> cfSi("{x} sinint(x)");
    EXPECT_EQ("sinint(x)", cfSi.format()) << "basic_function<TPC> sinint - format()";
    EXPECT_FLOAT_EQ(0.F, static_cast<float>(cfSi(TPC(0.,0.)).real())) << "basic_function<TPC> sinint - value";
    EXPECT_FLOAT_EQ(0.F, static_cast<float>(cfSi(TPC(0.,0.)).imag())) << "basic_function<TPC> sinint - value";
    EXPECT_FLOAT_EQ(1.10422265823558F, static_cast<float>(cfSi(TPC(1.,1.)).real())) << "basic_function<TPC> sinint - value";
    EXPECT_FLOAT_EQ(0.882453805007918F, static_cast<float>(cfSi(TPC(1.,1.)).imag())) << "basic_function<TPC> sinint - value";
    EXPECT_FLOAT_EQ(-0.946083070367183F, static_cast<float>(cfSi(TPC(-1.,0.)).real())) << "basic_function<TPC> sinint - value";
    EXPECT_FLOAT_EQ(0.F, static_cast<float>(cfSi(TPC(-1.,0.)).imag())) << "basic_function<TPC> sinint - value";
    
    EXPECT_EQ("sin(x)/x", cfSi.drv().format()) << "basic_function<TPC> sinint - drv - format()";
}

// Fcosint
TYPED_TEST(FunctionalTest, TestFcosint) {
    basic_function<TP> rfCi("{x} cosint(x)");
    EXPECT_EQ("cosint(x)", rfCi.format()) << "basic_function<TP> cosint - format()";
    EXPECT_FLOAT_EQ(-10.9357098000937F, static_cast<float>(rfCi(1.e-5))) << "basic_function<TP> cosint - value";
    EXPECT_FLOAT_EQ(0.422980828774865F, static_cast<float>(rfCi(2.))) << "basic_function<TP> cosint - value";
    EXPECT_EQ("cos(x)/x", rfCi.drv().format()) << "basic_function<TP> cosint - drv - format()";

    basic_function<TPC> cfCi("{x} cosint(x)");
    EXPECT_EQ("cosint(x)", cfCi.format()) << "basic_function<TPC> cosint - format()";

    EXPECT_FLOAT_EQ(0.337403922900968F, static_cast<float>(cfCi(TPC(-1.,0.)).real())) << "basic_function<TPC> cosint - value";
    EXPECT_FLOAT_EQ(3.14159265358979F, static_cast<float>(cfCi(TPC(-1.,0.)).imag())) << "basic_function<TPC> cosint - value";
    EXPECT_FLOAT_EQ(0.882172180555936F, static_cast<float>(cfCi(TPC(1.,1.)).real())) << "basic_function<TPC> cosint - value";
    EXPECT_FLOAT_EQ(0.287249133519956F, static_cast<float>(cfCi(TPC(1.,1.)).imag())) << "basic_function<TPC> cosint - value";
    
    EXPECT_EQ("cos(x)/x", cfCi.drv().format()) << "basic_function<TPC> cosint - drv - format()";
}

TYPED_TEST(FunctionalTest, TestFExpressions) {
    basic_function<TP> rf0;
    basic_function<TP> rfsin("{x} sin(x)");
    basic_function<TP> rfcos("{x} cos(x)");
    basic_function<TPC> cfsin("{x} sin(x)");
    basic_function<TPC> cfcos("{x} cos(x)");
    basic_function<TP> rfc ("7.77 ");
    basic_function<TPC> cfc ("(7.77, 8.88)");

    basic_function<TP> f1 ("{t} sin(t)^2 + cos(t)^2");
    EXPECT_EQ("0", (f1 * rf0).simp().format()) << "basic_function<TP> * basic_function<TP>";
    EXPECT_EQ(true, (f1 * rf0).simp() == basic_function<TP>("{t} 0")) << "basic_function<TP> * basic_function<TP>";
    
    basic_function<TP> rf_plus = rfsin + rfcos;
    EXPECT_EQ("sin(x)+cos(x)", rf_plus.format()) << "rf_plus.format";
    EXPECT_EQ(true, rf_plus == basic_function<TP>("{x} sin(x)+cos(x)")) << "rf_plus.format";
    
    basic_function<TP> rf_minus = rfsin - rfcos;
    EXPECT_EQ("sin(x)-cos(x)", rf_minus.format()) << "rf_minus.format";
    EXPECT_EQ(true, rf_minus == basic_function<TP>("{x} sin(x)-cos(x)")) << "rf_minus.format";
    
    basic_function<TP> rf_mult = rfsin * rfcos;
    EXPECT_EQ("sin(x)*cos(x)", rf_mult.format()) << "rf_mult.format";
    EXPECT_EQ(true, rf_mult == basic_function<TP>("{x} sin(x)*cos(x)")) << "rf_mult.format";
    
    basic_function<TP> rf_div = rfsin / rfcos;
    EXPECT_EQ("sin(x)/cos(x)", rf_div.format()) << "rf_div.format";
    EXPECT_EQ(true, rf_div == basic_function<TP>("{x} sin(x)/cos(x)")) << "rf_div.format";
    
    basic_function<TP> rf_power = rfsin ^ rfcos;
    EXPECT_EQ("sin(x)^cos(x)", rf_power.format()) << "rf_power.format";
    EXPECT_EQ(true, rf_power == basic_function<TP>("{x} sin(x)^cos(x)")) << "rf_power.format";
    
    basic_function<TP> rf_uminus = - rfsin;
    EXPECT_EQ("-sin(x)", rf_uminus.format()) << "rf_uminus.format";
    EXPECT_EQ(true, rf_uminus == basic_function<TP>("{x} -sin(x)")) << "rf_uminus.format";
    
    basic_function<TPC> cf_plus = cfsin + cfcos;
    EXPECT_EQ("sin(x)+cos(x)", cf_plus.format()) << "cf_plus.format";
    EXPECT_EQ(true, cf_plus == basic_function<TPC>("{x} sin(x)+cos(x)")) << "cf_plus.format";
    
    basic_function<TPC> cf_minus = cfsin - cfcos;
    EXPECT_EQ("sin(x)-cos(x)", cf_minus.format()) << "cf_minus.format";
    EXPECT_EQ(true, cf_minus == basic_function<TPC>("{x} sin(x)-cos(x)")) << "cf_minus.format";
    
    basic_function<TPC> cf_mult = cfsin * cfcos;
    EXPECT_EQ("sin(x)*cos(x)", cf_mult.format()) << "cf_mult.format";
    EXPECT_EQ(true, cf_mult == basic_function<TPC>("{x} sin(x)*cos(x)")) << "cf_mult.format";
    
    basic_function<TPC> cf_div = cfsin / cfcos;
    EXPECT_EQ("sin(x)/cos(x)", cf_div.format()) << "cf_div.format";
    EXPECT_EQ(true, cf_div == basic_function<TPC>("{x} sin(x)/cos(x)")) << "cf_div.format";
    
    basic_function<TPC> cf_power = cfsin ^ cfcos;
    EXPECT_EQ("sin(x)^cos(x)", cf_power.format()) << "cf_power.format";
    EXPECT_EQ(true, cf_power == basic_function<TPC>("{x} sin(x)^cos(x)")) << "cf_power.format";
    
    basic_function<TPC> cf_uminus = - cfsin;
    EXPECT_EQ("-sin(x)", cf_uminus.format()) << "cf_uminus.format";
    EXPECT_EQ(true, cf_uminus == basic_function<TPC>("{x} -sin(x)")) << "cf_uminus.format";
    
    rf_plus += rfcos;
    EXPECT_EQ("sin(x)+cos(x)+cos(x)", rf_plus.format()) << "rf_plus.format";
    EXPECT_EQ(true, rf_plus == basic_function<TP>("{x} sin(x)+cos(x)+cos(x)")) << "rf_plus.format";
    
    rf_plus.simp();
    EXPECT_EQ("2*cos(x)+sin(x)", rf_plus.format()) << "rf_plus.format";
    EXPECT_EQ(true, rf_plus == basic_function<TP>("{x} 2*cos(x)+sin(x)")) << "rf_plus.format";
    
    rf_plus -= rfcos;
    EXPECT_EQ("2*cos(x)+sin(x)-cos(x)", rf_plus.format()) << "rf_plus.format";
    EXPECT_EQ(true, rf_plus == basic_function<TP>("{x} 2*cos(x)+sin(x)-cos(x)")) << "rf_plus.format";
    
    rf_plus.simp();
    EXPECT_EQ("sin(x)+cos(x)", rf_plus.format()) << "rf_plus.format";
    EXPECT_EQ(true, rf_plus == basic_function<TP>("{x} sin(x)+cos(x)")) << "rf_plus.format";
    
    rf_div *= rfcos;
    EXPECT_EQ("sin(x)/cos(x)*cos(x)", rf_div.format()) << "rf_div.format";
    EXPECT_EQ(true, rf_div == basic_function<TP>("{x} sin(x)/cos(x)*cos(x)")) << "rf_div.format";
    
    rf_div.simp();
    EXPECT_EQ("sin(x)", rf_div.format()) << "rf_div.format";
    EXPECT_EQ(true, rf_div == basic_function<TP>("{x} sin(x)")) << "rf_div.format";
    
    rf_div /= rfsin;
    EXPECT_EQ("sin(x)/sin(x)", rf_div.format()) << "rf_div.format";
    EXPECT_EQ(true, rf_div == basic_function<TP>("{x} sin(x)/sin(x)")) << "rf_div.format";
    
    rf_div.simp();
    EXPECT_EQ("1", rf_div.format()) << "rf_div.format";
    EXPECT_EQ(true, rf_div == basic_function<TP>("{x} 1")) << "rf_div.format";
    
    cf_plus += cfcos;
    EXPECT_EQ("sin(x)+cos(x)+cos(x)", cf_plus.format()) << "cf_plus.format";
    EXPECT_EQ(true, cf_plus == basic_function<TPC>("{x} sin(x)+cos(x)+cos(x)")) << "cf_plus.format";
    
    cf_plus.simp();
    EXPECT_EQ("(2,0)*cos(x)+sin(x)", cf_plus.format()) << "cf_plus.format";
    EXPECT_EQ(true, cf_plus == basic_function<TPC>("{x} (2, 0)*cos(x)+sin(x)")) << "cf_plus.format";
    
    cf_plus -= cfcos;
    EXPECT_EQ("(2,0)*cos(x)+sin(x)-cos(x)", cf_plus.format()) << "cf_plus.format";
    EXPECT_EQ(true, cf_plus == basic_function<TPC>("{x} (2, 0)*cos(x)+sin(x)-cos(x)")) << "cf_plus.format";
    
    cf_plus.simp();
    EXPECT_EQ("sin(x)+cos(x)", cf_plus.format()) << "cf_plus.format";
    EXPECT_EQ(true, cf_plus == basic_function<TPC>("{x} sin(x)+cos(x)")) << "cf_plus.format";
    
    cf_div *= cfcos;
    EXPECT_EQ("sin(x)/cos(x)*cos(x)", cf_div.format()) << "cf_div.format";
    EXPECT_EQ(true, cf_div == basic_function<TPC>("{x} sin(x)/cos(x)*cos(x)")) << "cf_div.format";
    
    cf_div.simp();
    EXPECT_EQ("sin(x)", cf_div.format()) << "cf_div.format";
    EXPECT_EQ(true, cf_div == basic_function<TPC>("{x} sin(x)")) << "cf_div.format";
    
    cf_div /= cfsin;
    EXPECT_EQ("sin(x)/sin(x)", cf_div.format()) << "cf_div.format";
    EXPECT_EQ(true, cf_div == basic_function<TPC>("{x} sin(x)/sin(x)")) << "cf_div.format";
    
    cf_div.simp();
    EXPECT_EQ("(1,0)", cf_div.format()) << "cf_div.format";
    EXPECT_EQ(true, cf_div == basic_function<TPC>("{x} (1, 0)")) << "cf_div.format";
    
    rfc = 1.;
    EXPECT_EQ("1", rfc.format()) << "basic_function<TP> = treal";
    EXPECT_EQ("-1", (-rfc).format()) << "-basic_function<TP> = treal";
    rfc += 3;
    rfc.simp();
    EXPECT_EQ("4", rfc.format()) << "basic_function<TP> += treal";
    rfc -= 9;
    rfc.simp();
    EXPECT_EQ("(-5)", rfc.format()) << "basic_function<TP> -= treal";
    rfc *= 3;
    rfc.simp();
    EXPECT_EQ("(-15)", rfc.format()) << "basic_function<TP> *= treal";
    rfc /= 5;
    rfc.simp();
    EXPECT_EQ("(-3)", rfc.format()) << "basic_function<TP> /= treal";
    rfc ^= 2;
    rfc.simp();
    EXPECT_EQ("9", rfc.format()) << "basic_function<TP> ^= treal";
    
    cfc = TPC(1.,1.);
    EXPECT_EQ("(1,1)", cfc.format()) << "cfc = TPC";
    EXPECT_EQ("-(1,1)", (-cfc).format()) << "-cfc = TPC";
    cfc += TPC(3.,4.);
    cfc.simp();
    EXPECT_EQ("(4,5)", cfc.format()) << "basic_function<TPC> += TPC";
    cfc -= TPC(2.,1.);
    cfc.simp();
    EXPECT_EQ("(2,4)", cfc.format()) << "basic_function<TPC> -= TPC";
    cfc *= TPC(1.,3.);
    cfc.simp();
    EXPECT_EQ("(-10,10)", cfc.format()) << "basic_function<TPC> -= TPC";
    cfc /= TPC(10.,0.);
    cfc.simp();
    EXPECT_EQ("(-1,1)", cfc.format()) << "basic_function<TPC> -= TPC";

    basic_function<TP> rfc2(2);
    EXPECT_EQ(TP(11.), (rfc + rfc2)()) << "basic_function<TP> + basic_function<TP>";
    EXPECT_EQ(TP(7.), (rfc - rfc2)()) << "basic_function<TP> - basic_function<TP>";
    EXPECT_EQ(TP(18.), (rfc * rfc2)()) << "basic_function<TP> * basic_function<TP>";
    EXPECT_EQ(TP(4.5), (rfc / rfc2)()) << "basic_function<TP> / basic_function<TP>";
    EXPECT_EQ(TP(81.), (rfc ^ rfc2)()) << "basic_function<TP> ^ basic_function<TP>";
    
    EXPECT_EQ(TP(11.), (2. + rfc)()) << "treal + basic_function<TP>";
    EXPECT_EQ(TP(11.), (2 + rfc)()) << "treal + basic_function<TP>";
    EXPECT_EQ(TP(-7.), (2 - rfc)()) << "treal + basic_function<TP>";
    EXPECT_EQ(TP(18.), (2 * rfc)()) << "treal + basic_function<TP>";
    EXPECT_EQ(TP(0.2222222222222222), (2 / rfc)()) << "treal + basic_function<TP>";
    EXPECT_EQ(TP(512.), (2 ^ rfc)()) << "treal + basic_function<TP>";

    basic_function<TPC> cfc2 ("5.55");
    const TP r2 = 2.;
    const TPC c11 = TPC(1.,-1.);
    rfc2 = r2;
    cfc2 = c11;
    
    basic_function<TP> rf_self("{x} x");
    EXPECT_EQ(TP(-1.), rf_self.sat(rfc2)(-2.000001)) << "basic_function<TP>.sat - value";
    EXPECT_EQ(TP(0.), rf_self.sat(rfc2)(-2.)) << "basic_function<TP>.sat - value";
    EXPECT_EQ(TP(0.), rf_self.sat(rfc2)(2.)) << "basic_function<TP>.sat - value";
    EXPECT_EQ(TP(1.), rf_self.sat(rfc2)(2.00001)) << "basic_function<TP>.sat - value";
    basic_function<TPC> cf_self("{x} x");

    EXPECT_EQ(TPC(-1.,0.), cf_self.sat(cfc2)(TPC(-1.0001,1.))) << "basic_function<TPC>.sat - value";
    EXPECT_EQ(TPC(0.,0.), cf_self.sat(cfc2)(TPC(-1.,1.))) << "basic_function<TPC>.sat - value";
    EXPECT_EQ(TPC(0.,0.), cf_self.sat(cfc2)(TPC(1.,1.))) << "basic_function<TPC>.sat - value";
    EXPECT_EQ(TPC(1.,0.), cf_self.sat(cfc2)(TPC(1.0001,1.))) << "basic_function<TPC>.sat - value";
    
    rfc = rf_self.exp().simp();
    EXPECT_FLOAT_EQ(cfun_e<float>() * cfun_e<float>(), static_cast<float>(rfc(r2))) << "basic_function<TP>.exp - value";
    cfc = cf_self.exp().simp();
    EXPECT_EQ(std::exp(c11), cfc(c11)) << "basic_function<TPC>.exp - value";
    
    rfc = rf_self.sqrt().simp();
    EXPECT_EQ(std::sqrt(r2), rfc(r2)) << "basic_function<TP>.sqrt - value";
    cfc = cf_self.sqrt().simp();
    EXPECT_EQ(std::sqrt(c11), cfc(c11)) << "basic_function<TPC>.sqrt - value";
    
    rfc = rf_self.log().simp();
    EXPECT_EQ(std::log(r2), rfc(r2)) << "basic_function<TP>.log - value";
    cfc = cf_self.log().simp();
    EXPECT_EQ(std::log(c11), cfc(c11)) << "basic_function<TPC>.log - value";
    
    rfc = rf_self.log10().simp();
    EXPECT_EQ(std::log10(r2), rfc(r2)) << "basic_function<TP>.log10 - value";
    cfc = cf_self.log10().simp();
    EXPECT_EQ(std::log10(c11), cfc(c11)) << "basic_function<TPC>.log - value";
    
    rfc = rf_self.sin().simp();
    EXPECT_EQ(std::sin(r2), rfc(r2)) << "basic_function<TP>.sin - value";
    cfc = cf_self.sin().simp();
    EXPECT_EQ(std::sin(c11), cfc(c11)) << "basic_function<TPC>.sin - value";
    
    rfc = rf_self.cos().simp();
    EXPECT_EQ(std::cos(r2), rfc(r2)) << "basic_function<TP>.cos - value";
    cfc = cf_self.cos().simp();
    EXPECT_EQ(std::cos(c11), cfc(c11)) << "basic_function<TPC>.cos - value";
    
    rfc = rf_self.tan().simp();
    EXPECT_EQ(std::tan(r2), rfc(r2)) << "basic_function<TP>.tan - value";
    cfc = cf_self.tan().simp();
    EXPECT_FLOAT_EQ(static_cast<float>(std::tan(c11).real()),
                    static_cast<float>(cfc(c11).real())) << "basic_function<TPC>.tan - value";
    EXPECT_FLOAT_EQ(static_cast<float>(std::tan(c11).imag()),
                    static_cast<float>(cfc(c11).imag())) << "basic_function<TPC>.tan - value";
    
    rfc = rf_self.asin().simp();
    EXPECT_EQ(std::asin(1/r2), rfc(1/r2)) << "basic_function<TP>.asin - value";
    cfc = cf_self.asin().simp();
    EXPECT_EQ(ElementaryFunctions<TPC>::asin(c11), cfc(c11)) << "basic_function<TPC>.asin - value";
    
    rfc = rf_self.acos().simp();
    EXPECT_FLOAT_EQ(static_cast<float>(std::acos(1/r2)), static_cast<float>(rfc(1/r2))) << "basic_function<TP>.acos - value";
    cfc = cf_self.acos().simp();
    EXPECT_EQ(ElementaryFunctions<TPC>::acos(c11), cfc(c11)) << "basic_function<TPC>.acos - value";
    
    rfc = rf_self.atan().simp();
    EXPECT_EQ(std::atan(r2), rfc(r2)) << "basic_function<TP>.atan - value";
    cfc = cf_self.atan().simp();
    EXPECT_EQ(ElementaryFunctions<TPC>::atan(c11), cfc(c11)) << "basic_function<TPC>.atan - value";
    
    rfc = rf_self.sinh().simp();
    EXPECT_EQ(std::sinh(r2), rfc(r2)) << "basic_function<TP>.sinh - value";
    cfc = cf_self.sinh().simp();
    EXPECT_EQ(std::sinh(c11), cfc(c11)) << "basic_function<TPC>.sinh - value";
    
    rfc = rf_self.cosh().simp();
    EXPECT_FLOAT_EQ(static_cast<float>(std::cosh(r2)),
                    static_cast<float>(rfc(r2))) << "basic_function<TP>.cosh - value";
    cfc = cf_self.cosh().simp();
    EXPECT_EQ(std::cosh(c11), cfc(c11)) << "basic_function<TPC>.cosh - value";
    
    rfc = rf_self.tanh().simp();
    EXPECT_EQ(std::tanh(r2), rfc(r2)) << "basic_function<TP>.tanh - value";
    cfc = cf_self.tanh().simp();
    EXPECT_EQ(std::tanh(c11), cfc(c11)) << "basic_function<TPC>.tanh - value";
    
    rfc = rf_self.sinint().simp();
    EXPECT_EQ(ElementaryFunctions<TP>::sinint(r2, basic_cvmMachSp<TP>()), rfc(r2)) << "basic_function<TP>.sinint - value";
    cfc = cf_self.sinint().simp();
    EXPECT_EQ(ElementaryFunctions<TPC>::sinint(c11, basic_cvmMachSp<TP>()), cfc(c11)) << "basic_function<TPC>.sinint - value";

    rfc = rf_self.cosint().simp();
    EXPECT_EQ(ElementaryFunctions<TP>::cosint(r2, basic_cvmMachSp<TP>()), rfc(r2)) << "basic_function<TP>.cosint - value";
    cfc = cf_self.cosint().simp();
    EXPECT_EQ(ElementaryFunctions<TPC>::cosint(c11, basic_cvmMachSp<TP>()), cfc(c11)) << "basic_function<TPC>.cosint - value";

    rfc = rf_self.sign().simp();
    EXPECT_EQ(r2 > 0 ? TP(1.) : TP(-1.), rfc(r2)) << "basic_function<TP>.sign - value";
    EXPECT_EQ(TP(0.), rfc(0.)) << "basic_function<TP>.sign - value";
    cfc = cf_self.sign().simp();
    
    EXPECT_EQ(c11.real() > 0 ? TPC(1.,0.) : TPC(-1., 0.), cfc(c11)) << "basic_function<TPC>.sign - value";
    EXPECT_EQ(TPC(0., 0.), cfc(TPC(0.,0.))) << "basic_function<TPC>.sign - value";
    
    rfc = rf_self.abs().simp();
    EXPECT_EQ(std::fabs(r2), rfc(-r2)) << "basic_function<TP>.abs - value";
    EXPECT_EQ(std::fabs(r2), rfc(r2)) << "basic_function<TP>.abs - value";
    cfc = cf_self.abs().simp();
    EXPECT_EQ(TPC(std::abs(c11),0.), cfc(c11)) << "basic_function<TPC>.abs - value";

    rfc = rf_self.delta(basic_function<TP>(1.)).simp();
    EXPECT_EQ(TP(0.), rfc(r2)) << "basic_function<TP>.delta - value";
    EXPECT_EQ((std::numeric_limits<TP>::max)(), rfc(1)) << "basic_function<TP>.delta - value";
    cfc = cf_self.delta(basic_function<TPC>(TPC(1.,0.))).simp();
    EXPECT_EQ(TPC((std::numeric_limits<TP>::max)(),0.), cfc(c11)) << "basic_function<TPC>.delta - value";
    
    rfc = rf_self.iif(basic_function<TP>(3.), basic_function<TP>(4.)).simp();
    EXPECT_EQ(TP(3.), rfc(-1.)) << "basic_function<TP>.iif - value";
    EXPECT_EQ(TP(4.), rfc(0.)) << "basic_function<TP>.iif - value";
    EXPECT_EQ(TP(4.), rfc(1.)) << "basic_function<TP>.iif - value";
    
    cfc = cf_self.iif(basic_function<TPC>(3.), basic_function<TPC>(4.)).simp();
    EXPECT_EQ(TPC(3., 0.), cfc(TPC(-1.,0.))) << "basic_function<TPC>.iif - value";
    EXPECT_EQ(TPC(4., 0.), cfc(TPC(0.,0.))) << "basic_function<TPC>.iif - value";
    EXPECT_EQ(TPC(4., 0.), cfc(TPC(1.,0.))) << "basic_function<TPC>.iif - value";
}

TYPED_TEST(FunctionalTest, TestParameters) {
    std::vector<std::string> saVars;
    std::vector<std::string> saParameters;
    std::vector<std::string> saMeanings;
    
    saVars.push_back("t");
    saParameters.push_back("p");
    saMeanings.push_back("2");
    
    basic_function<TP> f2 (saVars, "sin(t)^p + cos(t)^p", saParameters, saMeanings);
    EXPECT_EQ(TP(1.), f2(-1.5)) << "basic_function<TP> sin(t)^p + cos(t)^p - value";
    
    basic_function<TPC> f2c (saVars, "sin(t)^p + cos(t)^p", saParameters, saMeanings);
    EXPECT_FLOAT_EQ(1.F, static_cast<float>(f2c(-1.5).real())) << "basic_function<TPC> sin(t)^p + cos(t)^p - value";
    EXPECT_NEAR(0.F, static_cast<float>(f2c(-1.5).imag()), sf<TP>()) << "basic_function<TPC> sin(t)^p + cos(t)^p - value";
}

// drv
TYPED_TEST(FunctionalTest, TestRealDrv) {
    basic_function<TP> f("{x, y} y*x+y^3");
    TP x[2];
    x[0] = 1.;
    x[1] = 2.;
    
    EXPECT_EQ("y", f.drv(0).format()) << "basic_function<TP> drv";
    EXPECT_EQ("x+3*y^2", f.drv(1).format()) << "basic_function<TP> drv";
    
    {
        std::ostringstream oss;
        oss << f.drv(0);
        EXPECT_EQ("{x,y} y", oss.str()) << "basic_function<TP> drv";
    }
    {
        std::ostringstream oss;
        oss << f.drv(0).drv(1);
        EXPECT_EQ("{x,y} 1", oss.str()) << "basic_function<TP> drv drv";
    }
    {
        std::ostringstream oss;
        oss << f.drv(1);
        EXPECT_EQ("{x,y} x+3*y^2", oss.str()) << "basic_function<TP> drv";
    }
    {
        std::ostringstream oss;
        oss << f.drv(1).drv(0);
        EXPECT_EQ("{x,y} 1", oss.str()) << "basic_function<TP> drv drv";
    }
    
    EXPECT_EQ(TP(2.), f.drv(0)(x)) << "basic_function<TP> drv value";
    EXPECT_EQ(TP(13.), f.drv(1)(x)) << "basic_function<TP> drv value";
}

TYPED_TEST(FunctionalTest, TestComplexDrv) {
    basic_function<TPC> f("{x, y} y*x+y^3");
    TPC x[2];
    x[0] = TPC(1.,-1.);
    x[1] = TPC(2.,-3.);
    
    EXPECT_EQ("y", f.drv(0).format()) << "basic_function<TPC> drv";
    EXPECT_EQ("x+(3,0)*y^(2,0)", f.drv(1).format()) << "basic_function<TPC> drv";
    
    {
        std::ostringstream oss;
        oss << f.drv(0);
        EXPECT_EQ("{x,y} y", oss.str()) << "basic_function<TPC> drv";
    }
    {
        std::ostringstream oss;
        oss << f.drv(0).drv(1);
        EXPECT_EQ("{x,y} (1,0)", oss.str()) << "basic_function<TPC> drv drv";
    }
    {
        std::ostringstream oss;
        oss << f.drv(1);
        EXPECT_EQ("{x,y} x+(3,0)*y^(2,0)", oss.str()) << "basic_function<TPC> drv";
    }
    {
        std::ostringstream oss;
        oss << f.drv(1).drv(0);
        EXPECT_EQ("{x,y} (1,0)", oss.str()) << "basic_function<TPC> drv drv";
    }
    
    EXPECT_NEAR(std::abs(TPC(2., 3.)), std::abs(f.drv(0)(x)), s<TP>()) << "basic_function<TPC> drv value";
    EXPECT_NEAR((x[0] + TPC(3.,0.) * x[1] * x[1]).real(), f.drv(1)(x).real(), s<TP>()) << "basic_function<TPC> drv value";
    EXPECT_NEAR((x[0] + TPC(3.,0.) * x[1] * x[1]).imag(), f.drv(1)(x).imag(), sp<TP>()) << "basic_function<TPC> drv value";
}
        
// assign number
TYPED_TEST(FunctionalTest, TestAssignNumber) {
    basic_function<TP> f;
    f = 2.2;
    EXPECT_EQ("2.2", f.format()) << "basic_function<TP> = number";
    
    basic_function<TPC> fc;
    fc = TPC(2.,-3.);
    EXPECT_EQ("(2,-3)", fc.format()) << "basic_function<TPC> = number";
}

// complex number
TYPED_TEST(FunctionalTest, TestComplexNumber) {
    basic_function<TPC> c1("(1, -2.3)");
    EXPECT_NEAR(std::abs(TPC(TP(1.), 2.3)), std::abs(c1()), s<TP>()) << "basic_function<TPC> (1, 2) - value";
    basic_function<TPC> c2("(-.1, 2.)");
    EXPECT_NEAR(std::abs(TPC(-.1,2.)), std::abs(c2()), s<TP>()) << "basic_function<TPC> (-.1, 2.) - value";
    basic_function<TPC> c3("{x, z} z+(3, -1.6)");
    TPC va[2];
    va[0] = TPC(.777,-1.888);
    va[1] = TPC(1.3,-2.1);
    EXPECT_FLOAT_EQ(4.3F, c3(va).real()) << "basic_function<TPC> {x, z} z+(3, -1.6) - value";
    EXPECT_FLOAT_EQ(-3.7F, c3(va).imag()) << "basic_function<TPC> {x, z} z+(3, -1.6) - value";
}

// rfvector
TYPED_TEST(FunctionalTest, TestRFVector) {
    string_array sa;
    sa.push_back ("{x, z} sign(x+2)");
    sa.push_back ("{x, z} z+3");
    
    basic_rfvector<TP> fa(sa);
    TP x[2];
    TP y[2];
    
    x[0] = -2.1;
    x[1] = 8.8;
    fa.value(x, y);
    
    EXPECT_EQ(TP(-1.), y[0]) << "rfvector - value";
    EXPECT_EQ(TP(11.8), y[1]) << "rfvector - value";
    
    basic_rvector<TP> yv = fa(x);
    EXPECT_EQ(TP(-1.), yv[CVM0]) << "rfvector - value";
    EXPECT_EQ(TP(11.8), yv[CVM0+1]) << "rfvector - value";
    
    basic_rfvector<TP> fcp(fa);
    basic_rfvector<TP> fcpcp;
    fcpcp << fcp;
    
    EXPECT_TRUE(fa == fcp) << "rfvector copy";
    EXPECT_TRUE(fa == fcpcp) << "rfvector copy";
    
    try {
        basic_function<TP> xf1 ("{a, b} a+3");
        fa /= xf1;
        FAIL() << "No exception about variables mismatch, rvector";
    } catch (cvmexception& ex) {
        EXPECT_EQ(CFUN_VARSDONTMATCH, ex.cause()) << "CFUN_VARSDONTMATCH exception cause";
    }
    
    basic_function<TP> xf1 ("{x, z} x+3");
    fa /= xf1;
    fcpcp /= xf1;
    
    std::stringstream s1, s2;
    s1 << fa;
    s2 << fcpcp;
    
    EXPECT_EQ(s1.str(), s2.str()) << "rfvector <<";
    EXPECT_EQ("{x,z} sign(x+2)/(x+3) {x,z} (z+3)/(x+3) \n", s1.str()) << "rfvector <<";
}

// rfvector drv
TYPED_TEST(FunctionalTest, TestRFVectorDrv) {
    string_array sa;
    sa.push_back ("{x, z} sin(x+2)^2");
    sa.push_back ("{x, z} z+3/x");
    
    basic_rfvector<TP> fv(sa);
    
    std::stringstream s1, s2;
    s1 << fv.drv(0);
    s2 << fv.drv(1);
    
    EXPECT_EQ("{x,z} cos(x+2)*2*sin(x+2) {x,z} (-3)/x^2 \n", s1.str()) << "rfvector drv";
    EXPECT_EQ("{x,z} 0 {x,z} 1 \n", s2.str()) << "rfvector drv";
}

// cfvector
TYPED_TEST(FunctionalTest, TestCFVector) {
    string_array sa;
    sa.push_back ("{x, z} sign(x+(2, 3))");
    sa.push_back ("{x, z} z+(3, -1.6)");
    
    basic_cfvector<TP,TPC> fa(sa);
    TPC x[2];
    TPC y[2];
    
    x[0] = TPC(-2.1,3.7);
    x[1] = TPC(8.8,-1.3);
    fa.value(x, y);
    
    EXPECT_NEAR(std::abs(TPC(-1.,0.)), std::abs(y[0]), s<TP>()) << "cfvector value";
    EXPECT_NEAR(std::abs(TPC(TP(11.8), 2.9)), std::abs(y[1]), s<TP>()) << "cfvector value";
    
    basic_cvector<TP,TPC> yv = fa(x);
    EXPECT_NEAR(std::abs(TPC(-1.,0.)), std::abs(yv[CVM0]), s<TP>()) << "cfvector value";
    EXPECT_NEAR(std::abs(TPC(TP(11.8), 2.9)), std::abs(yv[CVM0+1]), s<TP>()) << "cfvector value";
    
    basic_cfvector<TP,TPC> fcp(fa);
    basic_cfvector<TP,TPC> fcpcp;
    fcpcp << fcp;
    
    EXPECT_TRUE(fa == fcp) << "cfvector copy";
    EXPECT_TRUE(fa == fcpcp) << "cfvector copy";
    
    try {
        basic_function<TPC> xf1 ("{a, b} a+3");
        fa /= xf1;
        FAIL() << "No exception about variables mismatch, cvector";
    } catch (cvmexception& ex) {
        EXPECT_EQ(CFUN_VARSDONTMATCH, ex.cause()) << "CFUN_VARSDONTMATCH exception cause";
    }
    
    basic_function<TPC> xf1 ("{x, z} x+(3, 2.3)");
    fa /= xf1;
    fcpcp /= xf1;
    
    std::stringstream s1, s2;
    s1 << fa;
    s2 << fcpcp;
    
    EXPECT_EQ(s1.str(), s2.str()) << "cfvector <<";
    EXPECT_EQ("{x,z} sign(x+(2,3))/(x+(3,2.3)) {x,z} (z+(3,-1.6))/(x+(3,2.3)) \n", s1.str()) << "cfvector <<";
}

// cfvector drv
TYPED_TEST(FunctionalTest, TestCFVectorDrv) {
    string_array sa;
    sa.push_back ("{x, z} sin(x+2)^(2, -2.)");
    sa.push_back ("{x, z} z+(3, -1.)/x");
    
    basic_cfvector<TP,TPC> fv(sa);
    
    std::stringstream s1, s2;
    s1 << fv.drv(0);
    s2 << fv.drv(1);
    
    EXPECT_EQ("{x,z} cos(x+(2,0))*(2,-2)*sin(x+(2,0))^(1,-2) {x,z} (-3,1)/x^(2,0) \n", s1.str()) << "cfvector drv";
    EXPECT_EQ("{x,z} (0,0) {x,z} (1,0) \n", s2.str()) << "cfvector drv";
}
