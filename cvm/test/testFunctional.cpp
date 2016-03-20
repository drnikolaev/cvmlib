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
