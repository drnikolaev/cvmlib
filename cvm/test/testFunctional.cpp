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
