//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2016
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "cvm.h"
#include "blas.h"

CVM_NAMESPACE_BEG

template<>
CVM_API void
__gemm<float, basic_rmatrix<float> >
    (const basic_rmatrix<float>& ml, bool bTrans1,
     const basic_rmatrix<float>& mr, bool bTrans2,
     float dAlpha,
     basic_rmatrix<float>& mRes,
     float dBeta)
{
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
    SGEMM (bTrans1 ? Chars::pT() : Chars::pN(), 1, bTrans2 ? Chars::pT() : Chars::pN(), 1,
#else
    SGEMM (bTrans1 ? Chars::pT() : Chars::pN(),    bTrans2 ? Chars::pT() : Chars::pN(),
#endif
           bTrans1 ? ml._pn() : ml._pm(),
           bTrans2 ? mr._pm() : mr._pn(),
           bTrans1 ? ml._pm() : ml._pn(),
           &dAlpha,
           ml._pd(), ml._pldm(),
           mr._pd(), mr._pldm(), 
           &dBeta,
           mRes, mRes._pld());
}

template<>
CVM_API void
__gemm<double, basic_rmatrix<double> >
    (const basic_rmatrix<double>& ml, bool bTrans1,
     const basic_rmatrix<double>& mr, bool bTrans2,
     double dAlpha,
     basic_rmatrix<double>& mRes,
     double dBeta)
{
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
    DGEMM (bTrans1 ? Chars::pT() : Chars::pN(), 1, bTrans2 ? Chars::pT() : Chars::pN(), 1,
#else
    DGEMM (bTrans1 ? Chars::pT() : Chars::pN(),    bTrans2 ? Chars::pT() : Chars::pN(),
#endif
           bTrans1 ? ml._pn() : ml._pm(),
           bTrans2 ? mr._pm() : mr._pn(),
           bTrans1 ? ml._pm() : ml._pn(),
           &dAlpha, 
           ml._pd(), ml._pldm(),
           mr._pd(), mr._pldm(),
           &dBeta,
           mRes, mRes._pld());
}

template<>
CVM_API void
__symm<float, basic_srsmatrix<float>, basic_rmatrix<float> >
    (bool bLeft,
     const basic_srsmatrix<float>& ml,
     const basic_rmatrix<float>& mr, 
     float dAlpha,
     basic_rmatrix<float>& mRes,
     float dBeta)
{
    SSYMM (bLeft ? Chars::pL() : Chars::pR(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           mRes._pm(), mRes._pn(),
           &dAlpha,
           ml._pd(), ml._pld(),
           mr._pd(), mr._pld(),
           &dBeta,
           mRes, mRes._pld());
}

template<>
CVM_API void
__symm<double, basic_srsmatrix<double>, basic_rmatrix<double>  >
    (bool bLeft,
     const basic_srsmatrix<double>& ml,
     const basic_rmatrix<double>& mr, 
     double dAlpha,
     basic_rmatrix<double>& mRes,
     double dBeta)
{
    DSYMM (bLeft ? Chars::pL() : Chars::pR(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           mRes._pm(), mRes._pn(),
           &dAlpha,
           ml._pd(), ml._pld(),
           mr._pd(), mr._pld(),
           &dBeta,
           mRes, mRes._pld());
}

template <>
CVM_API void
__syrk<float, basic_srsmatrix<float> >
    (bool bTransp, 
    float alpha, tint k,
    const float* pA, tint ldA,
    float beta, basic_srsmatrix<float>& m)
{
    SSYRK (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           bTransp ? Chars::pT() : Chars::pN(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), &k, &alpha, pA, &ldA, &beta, m, m._pld());
}

template <>
CVM_API void
__syrk<double, basic_srsmatrix<double> >
    (bool bTransp, 
    double alpha, tint k,
    const double* pA, tint ldA,
    double beta, basic_srsmatrix<double>& m)
{
    DSYRK (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           bTransp ? Chars::pT() : Chars::pN(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), &k, &alpha, pA, &ldA, &beta, m, m._pld());
}

template <>
CVM_API void
__syr2k<float, basic_srsmatrix<float> >
    (bool bTransp, 
    float alpha, tint k,
    const float* pA, tint ldA,
    const float* pB, tint ldB,
    float beta, basic_srsmatrix<float>& m)
{
    SSYR2K (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           bTransp ? Chars::pT() : Chars::pN(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), &k, &alpha, pA, &ldA, pB, &ldB, &beta, m, m._pld());
}

template <>
CVM_API void
__syr2k<double, basic_srsmatrix<double> >
    (bool bTransp, 
    double alpha, tint k,
    const double* pA, tint ldA,
    const double* pB, tint ldB,
    double beta, basic_srsmatrix<double>& m)
{
    DSYR2K (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           bTransp ? Chars::pT() : Chars::pN(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), &k, &alpha, pA, &ldA, pB, &ldB, &beta, m, m._pld());
}

// QR Case 1: "economy" mode, A is (m x n) and Q is (m x n) and R is (n x n)
template <>
CVM_API void 
__qre<basic_rmatrix<float>, basic_srmatrix<float> >
    (const basic_rmatrix<float>& mArg, 
    basic_rmatrix<float>& mQ, 
    basic_srmatrix<float>& mR) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;
    basic_rvector<float> vTau (nK);

    tint lWork = -1;
    tint nOutInfo = 0;
    float dWork;

    // calculate size of workspace
    SGEQRF (&nM, &nN, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    SGEQRF (&nM, &nN, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-triangular R from overwritten A
    mR.vanish();
    for (tint row = CVM0; row < nK + CVM0; ++row)
        for (tint col = row; col < nN + CVM0; ++col)
            mR(row,col) = mQ(row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    SORGQR (&nM, &nK, &nK, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    SORGQR (&nM, &nK, &nK, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// QR Case 1: "economy" mode, A is (m x n) and Q is (m x n) and R is (n x n)
template <>
CVM_API void 
__qre<basic_rmatrix<double>, basic_srmatrix<double> >
    (const basic_rmatrix<double>& mArg, 
    basic_rmatrix<double>& mQ, 
    basic_srmatrix<double>& mR) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;
    basic_rvector<double> vTau (nK);

    tint lWork = -1;
    tint nOutInfo = 0;
    double dWork;

    // calculate size of workspace
    DGEQRF (&nM, &nN, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    DGEQRF (&nM, &nN, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-triangular R from overwritten A
    mR.vanish();
    for (tint row = CVM0; row < nK + CVM0; ++row)
        for (tint col = row; col < nN + CVM0; ++col)
            mR(row,col) = mQ(row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    DORGQR (&nM, &nK, &nK, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    DORGQR (&nM, &nK, &nK, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// QR Case 2: full mode, A is (m x n) and Q is (m x m) and R is (m x n)
template <>
CVM_API void 
__qrf<basic_rmatrix<float>, basic_srmatrix<float> >
    (const basic_rmatrix<float>& mArg, 
    basic_srmatrix<float>& mQ, 
    basic_rmatrix<float>& mR) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // unlike "economy" mode, we need a copy here since Q will be m x m and this may bigger than original A
    basic_rmatrix<float> mA (nM, nN <= nM ? nM : nN);

    // copy over argument matrix
    mA.assign (CVM0, CVM0, mArg);

    basic_rvector<float> vTau (nK);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    float dWork;

    // calculate size of workspace
    SGEQRF (&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    SGEQRF (&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-triangular R which is now m x n from overwritten A
    mR.vanish();
    for (row = CVM0; row < nK + CVM0; ++row)
        for (col = row; col < nN + CVM0; ++col)
            mR(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is m x m
    lWork = -1;
    SORGQR (&nM, &nM, &nK, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is m x m
    SORGQR (&nM, &nM, &nK, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // is Q big enough to have all conents of mA ?
    if (nN <= nM)
        mQ.assign (CVM0, CVM0, mA);
    else
        for (row = CVM0; row < nM + CVM0; ++row)
            for (col = CVM0; col < nM + CVM0; ++col)
                mQ(row,col) = mA(row,col);
}

// QR Case 2: full mode, A is (m x n) and Q is (m x m) and R is (m x n)
template <>
CVM_API void 
__qrf<basic_rmatrix<double>, basic_srmatrix<double> >
    (const basic_rmatrix<double>& mArg, 
    basic_srmatrix<double>& mQ, 
    basic_rmatrix<double>& mR) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // unlike "economy" mode, we need a copy here since Q will be m x m and this may bigger than original A
    basic_rmatrix<double> mA (nM, nN <= nM ? nM : nN);

    // copy over argument matrix
    mA.assign (CVM0, CVM0, mArg);

    basic_rvector<double> vTau (nK);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    double dWork;

    // calculate size of workspace
    DGEQRF (&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    DGEQRF (&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-triangular R which is now m x n from overwritten A
    mR.vanish();
    for (row = CVM0; row < nK + CVM0; ++row)
        for (col = row; col < nN + CVM0; ++col)
            mR(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is m x m
    lWork = -1;
    DORGQR (&nM, &nM, &nK, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is m x m
    DORGQR (&nM, &nM, &nK, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // is Q big enough to have all conents of mA ?
    if (nN <= nM)
        mQ.assign (CVM0, CVM0, mA);
    else
        for (row = CVM0; row < nM + CVM0; ++row)
            for (col = CVM0; col < nM + CVM0; ++col)
                mQ(row,col) = mA(row,col);
}


// RQ Case 1: "economy" mode, A is (m x n) and Q is (m x n) and R is (m x m)
// Following this definition: http://www.netlib.org/scalapack/slug/node57.html we assume that M <= N
template <>
CVM_API void 
__rqe<basic_rmatrix<float>, basic_srmatrix<float> >
    (const basic_rmatrix<float>& mArg, 
    basic_srmatrix<float>& mR,
    basic_rmatrix<float>& mQ) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;

    basic_rvector<float> vTau (nM);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    float dWork;

    // calculate size of workspace
    SGERQF (&nM, &nN, mQ, mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    SGERQF (&nM, &nN, mQ, mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-triangular R which is now m x m from overwritten A
    mR.vanish();
    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = row; col < nM + CVM0; ++col)
            mR(row,col) = mQ(row,nN - nM + col);

    // calculate size of workspace for finding Q that is m x n
    lWork = -1;
    SORGRQ (&nM, &nN, &nM, mQ, mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is m x n
    SORGRQ (&nM, &nN, &nM, mQ, mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// RQ Case 1: "economy" mode, A is (m x n) and Q is (m x n) and R is (m x m)
// Following this definition: http://www.netlib.org/scalapack/slug/node57.html we assume that M <= N
template <>
CVM_API void 
__rqe<basic_rmatrix<double>, basic_srmatrix<double> >
    (const basic_rmatrix<double>& mArg, 
    basic_srmatrix<double>& mR,
    basic_rmatrix<double>& mQ) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;

    basic_rvector<double> vTau (nM);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    double dWork;

    // calculate size of workspace
    DGERQF (&nM, &nN, mQ, mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    DGERQF (&nM, &nN, mQ, mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-triangular R which is now m x m from overwritten A
    mR.vanish();
    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = row; col < nM + CVM0; ++col)
            mR(row,col) = mQ(row,nN - nM + col);

    // calculate size of workspace for finding Q that is m x n
    lWork = -1;
    DORGRQ (&nM, &nN, &nM, mQ, mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is m x n
    DORGRQ (&nM, &nN, &nM, mQ, mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// RQ Case 2: full mode, A is (m x n) and Q is (n x n) and R is (m x n)
// Following this definition: http://www.netlib.org/scalapack/slug/node57.html we assume that M <= N
template <>
CVM_API void 
__rqf<basic_rmatrix<float>, basic_srmatrix<float> >
    (const basic_rmatrix<float>& mArg, 
    basic_rmatrix<float>& mR,
    basic_srmatrix<float>& mQ) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();

    // unlike "economy" mode, we need a copy here since Q will be n x n and this may be bigger than original A
    basic_rmatrix<float> mA (nN, nN);

    // copy over argument matrix
    mA.assign (CVM0, CVM0, mArg);

    basic_rvector<float> vTau (nM);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    float dWork;

    // calculate size of workspace
    SGERQF (&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    SGERQF (&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-trapezoidal (triangular) R which is now m x n from overwritten A
    mR.vanish();
    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = nN - nM + row; col < nN + CVM0; ++col)
            mR(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is n x n
    lWork = -1;
    SORGRQ (&nM, &nN, &nM, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is n x n
    SORGRQ (&nM, &nN, &nM, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = CVM0; col < nN + CVM0; ++col)
            mQ(nN - nM + row,col) = mA(row,col);
}

// RQ Case 2: full mode, A is (m x n) and Q is (n x n) and R is (m x n)
// Following this definition: http://www.netlib.org/scalapack/slug/node57.html we assume that M <= N
template <>
CVM_API void 
__rqf<basic_rmatrix<double>, basic_srmatrix<double> >
    (const basic_rmatrix<double>& mArg, 
    basic_rmatrix<double>& mR,
    basic_srmatrix<double>& mQ) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();

    // unlike "economy" mode, we need a copy here since Q will be n x n and this may be bigger than original A
    basic_rmatrix<double> mA (nN, nN);

    // copy over argument matrix
    mA.assign (CVM0, CVM0, mArg);

    basic_rvector<double> vTau (nM);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    double dWork;

    // calculate size of workspace
    DGERQF (&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    DGERQF (&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-trapezoidal (triangular) R which is now m x n from overwritten A
    mR.vanish();
    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = nN - nM + row; col < nN + CVM0; ++col)
            mR(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is n x n
    lWork = -1;
    DORGRQ (&nM, &nN, &nM, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is n x n
    DORGRQ (&nM, &nN, &nM, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = CVM0; col < nN + CVM0; ++col)
            mQ(nN - nM + row,col) = mA(row,col);
}


// LQ Case 1: "economy" mode, A is (m x n) and L is (m x m) and Q is (m x n)
template <>
CVM_API void 
__lqe<basic_rmatrix<float>, basic_srmatrix<float> >
    (const basic_rmatrix<float>& mArg, 
    basic_srmatrix<float>& mL, 
    basic_rmatrix<float>& mQ) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;
    basic_rvector<float> vTau (nK);

    tint lWork = -1;
    tint nOutInfo = 0;
    float dWork;

    // calculate size of workspace
    SGELQF (&nM, &nN, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    SGELQF (&nM, &nN, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower-triangular L from overwritten A
    mL.vanish();
    for (tint col = CVM0; col < nK + CVM0; ++col)
        for (tint row = col; row < nM + CVM0; ++row)
            mL(row,col) = mQ(row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    SORGLQ (&nK, &nN, &nK, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    SORGLQ (&nK, &nN, &nK, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

template <>
CVM_API void 
__lqe<basic_rmatrix<double>, basic_srmatrix<double> >
    (const basic_rmatrix<double>& mArg, 
    basic_srmatrix<double>& mL, 
    basic_rmatrix<double>& mQ) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;
    basic_rvector<double> vTau (nK);

    tint lWork = -1;
    tint nOutInfo = 0;
    double dWork;

    // calculate size of workspace
    DGELQF (&nM, &nN, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    DGELQF (&nM, &nN, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower-triangular L from overwritten A
    mL.vanish();
    for (tint col = CVM0; col < nK + CVM0; ++col)
        for (tint row = col; row < nM + CVM0; ++row)
            mL(row,col) = mQ(row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    DORGLQ (&nK, &nN, &nK, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    DORGLQ (&nK, &nN, &nK, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// LQ Case 2: full mode, A is (m x n) and L is (m x n) and Q is (n x n)
template <>
CVM_API void 
__lqf<basic_rmatrix<float>, basic_srmatrix<float> >
    (const basic_rmatrix<float>& mArg, 
    basic_rmatrix<float>& mL, 
    basic_srmatrix<float>& mQ) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // unlike "economy" mode, we need a copy here since Q will be n x n and this may bigger than original A
    basic_rmatrix<float> mA (nM <= nN ? nN : nM, nN);

    // copy over argument matrix
    mA.assign (CVM0, CVM0, mArg);

    basic_rvector<float> vTau (nK);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    float dWork;

    // calculate size of workspace
    SGELQF (&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    SGELQF (&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower-triangular L from overwritten A
    mL.vanish();
    for (tint col = CVM0; col < nK + CVM0; ++col)
        for (tint row = col; row < nM + CVM0; ++row)
            mL(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is m x m
    lWork = -1;
    SORGLQ (&nN, &nN, &nK, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is n x n
    SORGLQ (&nN, &nN, &nK, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // is Q big enough to have all conents of mA ?
    if (nM <= nN)
        mQ.assign (CVM0, CVM0, mA);
    else
        for (row = CVM0; row < nN + CVM0; ++row)
            for (col = CVM0; col < nN + CVM0; ++col)
                mQ(row,col) = mA(row,col);
}

// LQ Case 2: full mode, A is (m x n) and L is (m x n) and Q is (n x n)
template <>
CVM_API void 
__lqf<basic_rmatrix<double>, basic_srmatrix<double> >
    (const basic_rmatrix<double>& mArg, 
    basic_rmatrix<double>& mL, 
    basic_srmatrix<double>& mQ) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // unlike "economy" mode, we need a copy here since Q will be n x n and this may bigger than original A
    basic_rmatrix<double> mA (nM <= nN ? nN : nM, nN);

    // copy over argument matrix
    mA.assign (CVM0, CVM0, mArg);

    basic_rvector<double> vTau (nK);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    double dWork;

    // calculate size of workspace
    DGELQF (&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    DGELQF (&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower-triangular L from overwritten A
    mL.vanish();
    for (tint col = CVM0; col < nK + CVM0; ++col)
        for (tint row = col; row < nM + CVM0; ++row)
            mL(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is m x m
    lWork = -1;
    DORGLQ (&nN, &nN, &nK, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is n x n
    DORGLQ (&nN, &nN, &nK, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // is Q big enough to have all conents of mA ?
    if (nM <= nN)
        mQ.assign (CVM0, CVM0, mA);
    else
        for (row = CVM0; row < nN + CVM0; ++row)
            for (col = CVM0; col < nN + CVM0; ++col)
                mQ(row,col) = mA(row,col);
}

// QL Case 1: "economy" mode, A is (m x n) and Q is (m x n) and L is (n x n)
// Following this definition: http://www.netlib.org/scalapack/slug/node57.html we assume that M >= N
template <>
CVM_API void 
__qle<basic_rmatrix<float>, basic_srmatrix<float> >
    (const basic_rmatrix<float>& mArg, 
    basic_rmatrix<float>& mQ,
    basic_srmatrix<float>& mL) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;
    basic_rvector<float> vTau (nK);

    tint lWork = -1;
    tint nOutInfo = 0;
    float dWork;

    // calculate size of workspace
    SGEQLF (&nM, &nN, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    SGEQLF (&nM, &nN, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower-triangular L from overwritten A
    mL.vanish();
    for (tint col = CVM0; col < nN + CVM0; ++col)
        for (tint row = col; row < nK + CVM0; ++row)
            mL(row,col) = mQ(nM - nN + row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    SORGQL (&nM, &nN, &nK, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    SORGQL (&nM, &nN, &nK, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// QL Case 1: "economy" mode, A is (m x n) and Q is (m x n) and L is (n x n)
// Following this definition: http://www.netlib.org/scalapack/slug/node57.html we assume that M >= N
template <>
CVM_API void 
__qle<basic_rmatrix<double>, basic_srmatrix<double> >
    (const basic_rmatrix<double>& mArg, 
    basic_rmatrix<double>& mQ,
    basic_srmatrix<double>& mL) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;
    basic_rvector<double> vTau (nK);

    tint lWork = -1;
    tint nOutInfo = 0;
    double dWork;

    // calculate size of workspace
    DGEQLF (&nM, &nN, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    DGEQLF (&nM, &nN, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower-triangular L from overwritten A
    mL.vanish();
    for (tint col = CVM0; col < nN + CVM0; ++col)
        for (tint row = col; row < nK + CVM0; ++row)
            mL(row,col) = mQ(nM - nN + row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    DORGQL (&nM, &nN, &nK, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    DORGQL (&nM, &nN, &nK, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// QL Case 2: full mode, A is (m x n) and Q is (m x m) and L is (m x n)
// Following this definition: http://www.netlib.org/scalapack/slug/node57.html we assume that M >= N
template <>
CVM_API void 
__qlf<basic_rmatrix<float>, basic_srmatrix<float> >
    (const basic_rmatrix<float>& mArg, 
    basic_srmatrix<float>& mQ,
    basic_rmatrix<float>& mL) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();

    // unlike "economy" mode, we need a copy here since Q will be m x m and this may be bigger than original A
    basic_rmatrix<float> mA (nM, nM);

    // copy over argument matrix
    mA.assign (CVM0, CVM0, mArg);

    basic_rvector<float> vTau (nN);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    float dWork;

    // calculate size of workspace
    SGEQLF (&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    SGEQLF (&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower triangular L which is now m x n from overwritten A
    mL.vanish();
    for (col = CVM0; col < nN + CVM0; ++col)
        for (row = nM - nN + col; row < nM + CVM0; ++row)
            mL(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is n x n
    lWork = -1;
    SORGQL (&nM, &nN, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is n x n
    SORGQL (&nM, &nN, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = CVM0; col < nN + CVM0; ++col)
            mQ(row,nM - nN + col) = mA(row,col);
}

// QL Case 2: full mode, A is (m x n) and Q is (m x m) and L is (m x n)
// Following this definition: http://www.netlib.org/scalapack/slug/node57.html we assume that M >= N
template <>
CVM_API void 
__qlf<basic_rmatrix<double>, basic_srmatrix<double> >
    (const basic_rmatrix<double>& mArg, 
    basic_srmatrix<double>& mQ,
    basic_rmatrix<double>& mL) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();

    // unlike "economy" mode, we need a copy here since Q will be m x m and this may be bigger than original A
    basic_rmatrix<double> mA (nM, nM);

    // copy over argument matrix
    mA.assign (CVM0, CVM0, mArg);

    basic_rvector<double> vTau (nN);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    double dWork;

    // calculate size of workspace
    DGEQLF (&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    DGEQLF (&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower triangular L which is now m x n from overwritten A
    mL.vanish();
    for (col = CVM0; col < nN + CVM0; ++col)
        for (row = nM - nN + col; row < nM + CVM0; ++row)
            mL(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is n x n
    lWork = -1;
    DORGQL (&nM, &nN, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork);
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is n x n
    DORGQL (&nM, &nN, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = CVM0; col < nN + CVM0; ++col)
            mQ(row,nM - nN + col) = mA(row,col);
}

// LLS routines
template <>
CVM_API void
__gels<basic_rmatrix<float>, basic_rvector<float> >
    (bool transpose,
    basic_rmatrix<float>& mA,
    const basic_rmatrix<float>& mB,
    basic_rmatrix<float>& mX,  // output: will be resized anyway, so better pass it empty
    basic_rvector<float>& vErr) throw (cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nL = _cvm_min<tint>(nM, nN);
    const tint nK = _cvm_max<tint>(nM, nN);
    const tint nrhs = mB.nsize();
    tint lWork = -1;
    tint nOutInfo = 0;
    float dWork;
    const char* trans = transpose ? Chars::pT() : Chars::pN();

    mX.resize(nK, nrhs);   // first we might need extra space for residuals
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    SGELS (trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           &nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(), &dWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);

    SGELS (trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           &nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(), vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // collecting residuals if applicable
    vErr.set(0.F);
    if ((!transpose && nM > nN) || (transpose && nM < nN))
    {
        for (tint col = CVM0; col < nrhs + CVM0; col++)
        {
            for (tint row = nL + CVM0; row < nK + CVM0; row++)
            {
                vErr[col] += mX(row, col)*mX(row, col);
            }
        }
        mX.resize(nL, nrhs);
    }
}

template <>
CVM_API void
__gels<basic_rmatrix<double>, basic_rvector<double> >
    (bool transpose,
    basic_rmatrix<double>& mA,
    const basic_rmatrix<double>& mB,
    basic_rmatrix<double>& mX,  // output: will be resized anyway, so better pass it empty
    basic_rvector<double>& vErr) throw (cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nL = _cvm_min<tint>(nM, nN);
    const tint nK = _cvm_max<tint>(nM, nN);
    const tint nrhs = mB.nsize();
    tint lWork = -1;
    tint nOutInfo = 0;
    double dWork;
    const char* trans = transpose ? Chars::pT() : Chars::pN();

    mX.resize(nK, nrhs);   // first we might need extra space for residuals
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    DGELS (trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           &nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(), &dWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);

    DGELS (trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           &nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(), vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // collecting residuals if applicable
    vErr.set(0.);
    if ((!transpose && nM > nN) || (transpose && nM < nN))
    {
        for (tint col = CVM0; col < nrhs + CVM0; col++)
        {
            for (tint row = nL + CVM0; row < nK + CVM0; row++)
            {
                vErr[col] += mX(row, col)*mX(row, col);
            }
        }
        mX.resize(nL, nrhs);
    }
}

template <>
CVM_API void
__gelsy<float, basic_rmatrix<float> >
    (basic_rmatrix<float>& mA,
    const basic_rmatrix<float>& mB,
    basic_rmatrix<float>& mX,  // output: will be resized anyway, so better pass it empty
    float rcond,
    tint& rank) throw (cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nrhs = mB.nsize();
    const tint nK = _cvm_max<tint>(nM, nN);
    tint lWork = -1;
    tint nOutInfo = 0;
    basic_array<tint,tint> jpvt(nN);
    float dWork;

    mX.resize(nK, nrhs);   // we might need extra space here
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    SGELSY (&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            jpvt, &rcond, &rank, &dWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);

    SGELSY (&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            jpvt, &rcond, &rank, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    mX.resize(nN, nrhs);
}

template <>
CVM_API void
__gelsy<double, basic_rmatrix<double> >
    (basic_rmatrix<double>& mA,
    const basic_rmatrix<double>& mB,
    basic_rmatrix<double>& mX,  // output: will be resized anyway, so better pass it empty
    double rcond,
    tint& rank) throw (cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nrhs = mB.nsize();
    const tint nK = _cvm_max<tint>(nM, nN);
    tint lWork = -1;
    tint nOutInfo = 0;
    basic_array<tint,tint> jpvt(nN);
    double dWork;

    mX.resize(nK, nrhs);   // first we might need extra space for residuals
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    DGELSY (&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            jpvt, &rcond, &rank, &dWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);

    DGELSY (&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            jpvt, &rcond, &rank, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    mX.resize(nN, nrhs);
}

template <>
CVM_API void
__gelss<float, basic_rvector<float>, basic_rmatrix<float> >
    (basic_rmatrix<float>& mA,
    const basic_rmatrix<float>& mB,
    basic_rmatrix<float>& mX,  // output: will be resized anyway, so better pass it empty
    float rcond,
    basic_rvector<float>& vSV, // incr=1 required
    tint& rank) throw (cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nrhs = mB.nsize();
    const tint nK = _cvm_max<tint>(nM, nN);
    tint lWork = -1;
    tint nOutInfo = 0;
    float dWork;

    mX.resize(nK, nrhs);   // we might need extra space here
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    SGELSS (&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            vSV, &rcond, &rank, &dWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);

    SGELSS (&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            vSV, &rcond, &rank, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    mX.resize(nN, nrhs);
}

template <>
CVM_API void
__gelss<double, basic_rvector<double>, basic_rmatrix<double> >
    (basic_rmatrix<double>& mA,
    const basic_rmatrix<double>& mB,
    basic_rmatrix<double>& mX,  // output: will be resized anyway, so better pass it empty
    double rcond,
    basic_rvector<double>& vSV, // incr=1 required
    tint& rank) throw (cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nrhs = mB.nsize();
    const tint nK = _cvm_max<tint>(nM, nN);
    tint lWork = -1;
    tint nOutInfo = 0;
    double dWork;

    mX.resize(nK, nrhs);   // we might need extra space here
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    DGELSS (&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            vSV, &rcond, &rank, &dWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);

    DGELSS (&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            vSV, &rcond, &rank, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    mX.resize(nN, nrhs);
}

template <>
CVM_API void
__gelsd<float, basic_rvector<float>, basic_rmatrix<float> >
    (basic_rmatrix<float>& mA,
    const basic_rmatrix<float>& mB,
    basic_rmatrix<float>& mX,  // output: will be resized anyway, so better pass it empty
    float rcond,
    basic_rvector<float>& vSV, // incr=1 required
    tint& rank) throw (cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nrhs = mB.nsize();
    const tint nK = _cvm_max<tint>(nM, nN);
    tint lWork = -1;
    tint nOutInfo = 0;
    float dWork;
    tint iWork = -1;

    mX.resize(nK, nrhs);   // we might need extra space here
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    SGELSD (&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            vSV, &rcond, &rank, &dWork, &lWork, &iWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);
    basic_array<tint,tint> viWork(iWork);

    SGELSD (&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            vSV, &rcond, &rank, vWork, &lWork, viWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    mX.resize(nN, nrhs);
}

template <>
CVM_API void
__gelsd<double, basic_rvector<double>, basic_rmatrix<double> >
    (basic_rmatrix<double>& mA,
    const basic_rmatrix<double>& mB,
    basic_rmatrix<double>& mX,  // output: will be resized anyway, so better pass it empty
    double rcond,
    basic_rvector<double>& vSV, // incr=1 required
    tint& rank) throw (cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nrhs = mB.nsize();
    const tint nK = _cvm_max<tint>(nM, nN);
    tint lWork = -1;
    tint nOutInfo = 0;
    double dWork;
    tint iWork = -1;

    mX.resize(nK, nrhs);   // we might need extra space here
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    DGELSD (&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            vSV, &rcond, &rank, &dWork, &lWork, &iWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);
    basic_array<tint,tint> viWork(iWork);

    DGELSD (&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            vSV, &rcond, &rank, vWork, &lWork, viWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    mX.resize(nN, nrhs);
}

CVM_NAMESPACE_END
