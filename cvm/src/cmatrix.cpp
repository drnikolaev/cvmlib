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

//! @cond SPECIALIZATIONS
template<>
CVM_API void
__gemm<std::complex<float>, basic_cmatrix<float, std::complex<float> > >
    (const basic_cmatrix<float, std::complex<float> >& ml, bool bTrans1,
     const basic_cmatrix<float, std::complex<float> >& mr, bool bTrans2,
     std::complex<float> dAlpha, 
     basic_cmatrix<float, std::complex<float> >& mRes, 
     std::complex<float> dBeta)
{
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
    CGEMM(bTrans1 ? Chars::pC() : Chars::pN(), 1, bTrans2 ? Chars::pC() : Chars::pN(), 1,
#else
    CGEMM(bTrans1 ? Chars::pC() : Chars::pN(),    bTrans2 ? Chars::pC() : Chars::pN(),
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
__gemm<std::complex<double>, basic_cmatrix<double, std::complex<double> > >
    (const basic_cmatrix<double, std::complex<double> >& ml, bool bTrans1,
     const basic_cmatrix<double, std::complex<double> >& mr, bool bTrans2,
     std::complex<double> dAlpha,
     basic_cmatrix<double, std::complex<double> >& mRes,
     std::complex<double> dBeta)
{
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
    ZGEMM(bTrans1 ? Chars::pC() : Chars::pN(), 1, bTrans2 ? Chars::pC() : Chars::pN(), 1,
#else
    ZGEMM(bTrans1 ? Chars::pC() : Chars::pN(),    bTrans2 ? Chars::pC() : Chars::pN(),
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
__hemm<std::complex<float>, basic_schmatrix<float, std::complex<float> >, basic_cmatrix<float, std::complex<float> > >
    (bool bLeft,
     const basic_schmatrix<float, std::complex<float> >& ml,
     const basic_cmatrix<float, std::complex<float> >& mr, 
     std::complex<float> dAlpha,
     basic_cmatrix<float, std::complex<float> >& mRes,
     std::complex<float> dBeta)
{
    CHEMM(bLeft ? Chars::pL() : Chars::pR(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
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
__hemm<std::complex<double>, basic_schmatrix<double, std::complex<double> >, basic_cmatrix<double, std::complex<double> > >
    (bool bLeft,
     const basic_schmatrix<double, std::complex<double> >& ml,
     const basic_cmatrix<double, std::complex<double> >& mr, 
     std::complex<double> dAlpha,
     basic_cmatrix<double, std::complex<double> >& mRes,
     std::complex<double> dBeta)
{
    ZHEMM(bLeft ? Chars::pL() : Chars::pR(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
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
__herk<float, std::complex<float>, basic_schmatrix<float, std::complex<float> > >
    (bool bTransp, 
    float alpha, tint k,
    const std::complex<float>* pA, tint ldA,
    float beta, basic_schmatrix<float, std::complex<float> >& m)
{
    CHERK(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          bTransp ? Chars::pC() : Chars::pN(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          m._pm(), &k, &alpha, pA, &ldA, &beta, m, m._pld());
}

template <>
CVM_API void
__herk<double, std::complex<double>, basic_schmatrix<double, std::complex<double> > >
    (bool bTransp, 
    double alpha, tint k,
    const std::complex<double>* pA, tint ldA,
    double beta, basic_schmatrix<double, std::complex<double> >& m)
{
    ZHERK(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          bTransp ? Chars::pC() : Chars::pN(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          m._pm(), &k, &alpha, pA, &ldA, &beta, m, m._pld());
}

template <>
CVM_API void
__her2k<float, std::complex<float>, basic_schmatrix<float, std::complex<float> > >
    (bool bTransp, 
    std::complex<float> alpha, tint k,
    const std::complex<float>* pA, tint ldA,
    const std::complex<float>* pB, tint ldB,
    float beta, basic_schmatrix<float, std::complex<float> >& m)
{
    CHER2K(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           bTransp ? Chars::pC() : Chars::pN(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), &k, &alpha, pA, &ldA, pB, &ldB, &beta, m, m._pld());
}

template <>
CVM_API void
__her2k<double, std::complex<double>, basic_schmatrix<double, std::complex<double> > >
    (bool bTransp, 
    std::complex<double> alpha, tint k,
    const std::complex<double>* pA, tint ldA,
    const std::complex<double>* pB, tint ldB,
    double beta, basic_schmatrix<double, std::complex<double> >& m)
{
    ZHER2K(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           bTransp ? Chars::pC() : Chars::pN(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), &k, &alpha, pA, &ldA, pB, &ldB, &beta, m, m._pld());
}

// QR Case 1: economy mode, A is (m x n) and Q is (m x n) and R is (n x n)
template <>
CVM_API void 
__qre<basic_cmatrix<float, std::complex<float> >, basic_scmatrix<float, std::complex<float> > >
    (const basic_cmatrix<float, std::complex<float> >& mArg, 
    basic_cmatrix<float, std::complex<float> >& mQ,
    basic_scmatrix<float, std::complex<float> >& mR) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;
    basic_cvector<float, std::complex<float> > vTau(nK);

    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<float> dWork;

    // calculate size of workspace
    CGEQRF(&nM, &nN, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    CGEQRF(&nM, &nN, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-triangular R from overwritten A
    mR.vanish();
    for (tint row = CVM0; row < nK + CVM0; ++row)
        for (tint col = row; col < nN + CVM0; ++col)
            mR(row,col) = mQ(row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    CUNGQR(&nM, &nK, &nK, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    CUNGQR(&nM, &nK, &nK, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// QR Case 1: economy mode, A is (m x n) and Q is (m x n) and R is (n x n)
template <>
CVM_API void 
__qre<basic_cmatrix<double, std::complex<double> >, basic_scmatrix<double, std::complex<double> > >
    (const basic_cmatrix<double, std::complex<double> >& mArg, 
    basic_cmatrix<double, std::complex<double> >& mQ,
    basic_scmatrix<double, std::complex<double> >& mR) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;
    basic_cvector<double, std::complex<double> > vTau(nK);

    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<double> dWork;

    // calculate size of workspace
    ZGEQRF(&nM, &nN, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    ZGEQRF(&nM, &nN, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-triangular R from overwritten A
    mR.vanish();
    for (tint row = CVM0; row < nK + CVM0; ++row)
        for (tint col = row; col < nN + CVM0; ++col)
            mR(row,col) = mQ(row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    ZUNGQR(&nM, &nK, &nK, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    ZUNGQR(&nM, &nK, &nK, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// QR Case 2: full mode, A is (m x n) and Q is (m x m) and R is (m x n)
template <>
CVM_API void 
__qrf<basic_cmatrix<float, std::complex<float> >, basic_scmatrix<float, std::complex<float> > >
    (const basic_cmatrix<float, std::complex<float> >& mArg, 
    basic_scmatrix<float, std::complex<float> >& mQ,
    basic_cmatrix<float, std::complex<float> >& mR) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // unlike economy mode, we need a copy here since Q will be m x m and this may be bigger than original A
    basic_cmatrix<float, std::complex<float> > mA(nM, nN <= nM ? nM : nN);

    // copy over argument matrix
    mA.assign(CVM0, CVM0, mArg);

    basic_cvector<float, std::complex<float> > vTau(nK);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<float> dWork;

    // calculate size of workspace
    CGEQRF(&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    CGEQRF(&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-triangular R which is now m x n from overwritten A
    mR.vanish();
    for (row = CVM0; row < nK + CVM0; ++row)
        for (col = row; col < nN + CVM0; ++col)
            mR(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is m x m
    lWork = -1;
    CUNGQR(&nM, &nM, &nK, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is m x m
    CUNGQR(&nM, &nM, &nK, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // is Q big enough to have all conents of mA ?
    if (nN <= nM)
        mQ.assign(CVM0, CVM0, mA);
    else
        for (row = CVM0; row < nM + CVM0; ++row)
            for (col = CVM0; col < nM + CVM0; ++col)
                mQ(row,col) = mA(row,col);
}

// QR Case 2: full mode, A is (m x n) and Q is (m x m) and R is (m x n)
template <>
CVM_API void 
__qrf<basic_cmatrix<double, std::complex<double> >, basic_scmatrix<double, std::complex<double> > >
    (const basic_cmatrix<double, std::complex<double> >& mArg, 
    basic_scmatrix<double, std::complex<double> >& mQ,
    basic_cmatrix<double, std::complex<double> >& mR) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // unlike economy mode, we need a copy here since Q will be m x m and this may be bigger than original A
    basic_cmatrix<double, std::complex<double> > mA(nM, nN <= nM ? nM : nN);

    // copy over argument matrix
    mA.assign(CVM0, CVM0, mArg);

    basic_cvector<double, std::complex<double> > vTau(nK);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<double> dWork;

    // calculate size of workspace
    ZGEQRF(&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    ZGEQRF(&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-triangular R which is now m x n from overwritten A
    mR.vanish();
    for (row = CVM0; row < nK + CVM0; ++row)
        for (col = row; col < nN + CVM0; ++col)
            mR(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is m x m
    lWork = -1;
    ZUNGQR(&nM, &nM, &nK, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is m x m
    ZUNGQR(&nM, &nM, &nK, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // is Q big enough to have all conents of mA ?
    if (nN <= nM)
        mQ.assign(CVM0, CVM0, mA);
    else
        for (row = CVM0; row < nM + CVM0; ++row)
            for (col = CVM0; col < nM + CVM0; ++col)
                mQ(row,col) = mA(row,col);
}

// RQ Case 1: economy mode, A is (m x n) and Q is (m x n) and R is (m x m)
// Following this definition: http://www.netlib.org/scalapack/slug/node57.html we assume that M <= N
template <>
CVM_API void 
__rqe<basic_cmatrix<float, std::complex<float> >, basic_scmatrix<float, std::complex<float> > >
    (const basic_cmatrix<float, std::complex<float> >& mArg, 
    basic_scmatrix<float, std::complex<float> >& mR,
    basic_cmatrix<float, std::complex<float> >& mQ) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;

    basic_cvector<float, std::complex<float> > vTau(nM);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<float> dWork;

    // calculate size of workspace
    CGERQF(&nM, &nN, mQ, mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    CGERQF(&nM, &nN, mQ, mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-triangular R which is now m x m from overwritten A
    mR.vanish();
    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = row; col < nM + CVM0; ++col)
            mR(row,col) = mQ(row,nN - nM + col);

    // calculate size of workspace for finding Q that is m x n
    lWork = -1;
    CUNGRQ(&nM, &nN, &nM, mQ, mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is m x n
    CUNGRQ(&nM, &nN, &nM, mQ, mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// RQ Case 1: economy mode, A is (m x n) and Q is (m x n) and R is (m x m)
// Following this definition: http://www.netlib.org/scalapack/slug/node57.html we assume that M <= N
template <>
CVM_API void 
__rqe<basic_cmatrix<double, std::complex<double> >, basic_scmatrix<double, std::complex<double> > >
    (const basic_cmatrix<double, std::complex<double> >& mArg, 
    basic_scmatrix<double, std::complex<double> >& mR,
    basic_cmatrix<double, std::complex<double> >& mQ) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;

    basic_cvector<double, std::complex<double> > vTau(nM);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<double> dWork;

    // calculate size of workspace
    ZGERQF(&nM, &nN, mQ, mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    ZGERQF(&nM, &nN, mQ, mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-triangular R which is now m x m from overwritten A
    mR.vanish();
    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = row; col < nM + CVM0; ++col)
            mR(row,col) = mQ(row,nN - nM + col);

    // calculate size of workspace for finding Q that is m x n
    lWork = -1;
    ZUNGRQ(&nM, &nN, &nM, mQ, mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is m x n
    ZUNGRQ(&nM, &nN, &nM, mQ, mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// RQ Case 2: full mode, A is (m x n) and Q is (n x n) and R is (m x n)
// Following this definition: http://www.netlib.org/scalapack/slug/node57.html we assume that M <= N
template <>
CVM_API void 
__rqf<basic_cmatrix<float, std::complex<float> >, basic_scmatrix<float, std::complex<float> > >
    (const basic_cmatrix<float, std::complex<float> >& mArg, 
    basic_cmatrix<float, std::complex<float> >& mR,
    basic_scmatrix<float, std::complex<float> >& mQ) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();

    // unlike economy mode, we need a copy here since Q will be n x n and this may be bigger than original A
    basic_cmatrix<float, std::complex<float> > mA(nN, nN);

    // copy over argument matrix
    mA.assign(CVM0, CVM0, mArg);

    basic_cvector<float, std::complex<float> > vTau(nM);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<float> dWork;

    // calculate size of workspace
    CGERQF(&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    CGERQF(&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-trapezoidal (triangular) R which is now m x n from overwritten A
    mR.vanish();
    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = nN - nM + row; col < nN + CVM0; ++col)
            mR(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is n x n
    lWork = -1;
    CUNGRQ(&nM, &nN, &nM, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is n x n
    CUNGRQ(&nM, &nN, &nM, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = CVM0; col < nN + CVM0; ++col)
            mQ(nN - nM + row,col) = mA(row,col);
}

// RQ Case 2: full mode, A is (m x n) and Q is (n x n) and R is (m x n)
// Following this definition: http://www.netlib.org/scalapack/slug/node57.html we assume that M <= N
template <>
CVM_API void 
__rqf<basic_cmatrix<double, std::complex<double> >, basic_scmatrix<double, std::complex<double> > >
    (const basic_cmatrix<double, std::complex<double> >& mArg, 
    basic_cmatrix<double, std::complex<double> >& mR,
    basic_scmatrix<double, std::complex<double> >& mQ) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();

    // unlike economy mode, we need a copy here since Q will be n x n and this may be bigger than original A
    basic_cmatrix<double, std::complex<double> > mA(nN, nN);

    // copy over argument matrix
    mA.assign(CVM0, CVM0, mArg);

    basic_cvector<double, std::complex<double> > vTau(nM);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<double> dWork;

    // calculate size of workspace
    ZGERQF(&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    ZGERQF(&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get upper-trapezoidal (triangular) R which is now m x n from overwritten A
    mR.vanish();
    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = nN - nM + row; col < nN + CVM0; ++col)
            mR(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is n x n
    lWork = -1;
    ZUNGRQ(&nM, &nN, &nM, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is n x n
    ZUNGRQ(&nM, &nN, &nM, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = CVM0; col < nN + CVM0; ++col)
            mQ(nN - nM + row,col) = mA(row,col);
}

// LQ Case 1: "economy" mode, A is (m x n) and L is (m x m) and Q is (m x n)
template <>
CVM_API void 
__lqe<basic_cmatrix<float, std::complex<float> >, basic_scmatrix<float, std::complex<float> > >
    (const basic_cmatrix<float, std::complex<float> >& mArg, 
    basic_scmatrix<float, std::complex<float> >& mL,
    basic_cmatrix<float, std::complex<float> >& mQ) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;
    basic_cvector<float, std::complex<float> > vTau(nK);

    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<float> dWork;

    // calculate size of workspace
    CGELQF(&nM, &nN, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    CGELQF(&nM, &nN, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower-triangular L from overwritten A
    mL.vanish();
    for (tint col = CVM0; col < nK + CVM0; ++col)
        for (tint row = col; row < nM + CVM0; ++row)
            mL(row,col) = mQ(row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    CUNGLQ(&nK, &nN, &nK, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    CUNGLQ(&nK, &nN, &nK, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// LQ Case 1: "economy" mode, A is (m x n) and L is (m x m) and Q is (m x n)
template <>
CVM_API void 
__lqe<basic_cmatrix<double, std::complex<double> >, basic_scmatrix<double, std::complex<double> > >
    (const basic_cmatrix<double, std::complex<double> >& mArg, 
    basic_scmatrix<double, std::complex<double> >& mL,
    basic_cmatrix<double, std::complex<double> >& mQ) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;
    basic_cvector<double, std::complex<double> > vTau(nK);

    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<double> dWork;

    // calculate size of workspace
    ZGELQF(&nM, &nN, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    ZGELQF(&nM, &nN, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower-triangular L from overwritten A
    mL.vanish();
    for (tint col = CVM0; col < nK + CVM0; ++col)
        for (tint row = col; row < nM + CVM0; ++row)
            mL(row,col) = mQ(row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    ZUNGLQ(&nK, &nN, &nK, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    ZUNGLQ(&nK, &nN, &nK, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}

// LQ Case 2: full mode, A is (m x n) and L is (m x n) and Q is (n x n)
template <>
CVM_API void 
__lqf<basic_cmatrix<float, std::complex<float> >, basic_scmatrix<float, std::complex<float> > >
    (const basic_cmatrix<float, std::complex<float> >& mArg, 
    basic_cmatrix<float, std::complex<float> >& mL,
    basic_scmatrix<float, std::complex<float> >& mQ) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // unlike "economy" mode, we need a copy here since Q will be n x n and this may bigger than original A
    basic_cmatrix<float, std::complex<float> > mA(nM <= nN ? nN : nM, nN);

    // copy over argument matrix
    mA.assign(CVM0, CVM0, mArg);

    basic_cvector<float, std::complex<float> > vTau(nK);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<float> dWork;

    // calculate size of workspace
    CGELQF(&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    CGELQF(&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower-triangular L from overwritten A
    mL.vanish();
    for (col = CVM0; col < nK + CVM0; ++col)
        for (row = col; row < nM + CVM0; ++row)
            mL(row,col) = mA(row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    CUNGLQ(&nN, &nN, &nK, mA._pd(), mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    CUNGLQ(&nN, &nN, &nK, mA._pd(), mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // is Q big enough to have all conents of mA ?
    if (nM <= nN)
        mQ.assign(CVM0, CVM0, mA);
    else
        for (row = CVM0; row < nN + CVM0; ++row)
            for (col = CVM0; col < nN + CVM0; ++col)
                mQ(row,col) = mA(row,col);
}

// LQ Case 2: full mode, A is (m x n) and L is (m x n) and Q is (n x n)
template <>
CVM_API void 
__lqf<basic_cmatrix<double, std::complex<double> >, basic_scmatrix<double, std::complex<double> > >
    (const basic_cmatrix<double, std::complex<double> >& mArg, 
    basic_cmatrix<double, std::complex<double> >& mL,
    basic_scmatrix<double, std::complex<double> >& mQ) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // unlike "economy" mode, we need a copy here since Q will be n x n and this may bigger than original A
    basic_cmatrix<double, std::complex<double> > mA(nM <= nN ? nN : nM, nN);

    // copy over argument matrix
    mA.assign(CVM0, CVM0, mArg);

    basic_cvector<double, std::complex<double> > vTau(nK);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<double> dWork;

    // calculate size of workspace
    ZGELQF(&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    ZGELQF(&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower-triangular L from overwritten A
    mL.vanish();
    for (col = CVM0; col < nK + CVM0; ++col)
        for (row = col; row < nM + CVM0; ++row)
            mL(row,col) = mA(row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    ZUNGLQ(&nN, &nN, &nK, mA._pd(), mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    ZUNGLQ(&nN, &nN, &nK, mA._pd(), mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // is Q big enough to have all conents of mA ?
    if (nM <= nN)
        mQ.assign(CVM0, CVM0, mA);
    else
        for (row = CVM0; row < nN + CVM0; ++row)
            for (col = CVM0; col < nN + CVM0; ++col)
                mQ(row,col) = mA(row,col);
}


// QL Case 1: "economy" mode, A is (m x n) and Q is (m x n) and L is (n x n)
template <>
CVM_API void 
__qle<basic_cmatrix<float, std::complex<float> >, basic_scmatrix<float, std::complex<float> > >
    (const basic_cmatrix<float, std::complex<float> >& mArg, 
    basic_cmatrix<float, std::complex<float> >& mQ,
    basic_scmatrix<float, std::complex<float> >& mL) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;
    basic_cvector<float, std::complex<float> > vTau(nK);

    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<float> dWork;

    // calculate size of workspace
    CGEQLF(&nM, &nN, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    CGEQLF(&nM, &nN, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower-triangular L from overwritten A
    mL.vanish();
    for (tint col = CVM0; col < nN + CVM0; ++col)
        for (tint row = col; row < nK + CVM0; ++row)
            mL(row,col) = mQ(nM - nN + row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    CUNGQL(&nM, &nN, &nK, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    CUNGQL(&nM, &nN, &nK, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}


// QL Case 1: "economy" mode, A is (m x n) and Q is (m x n) and L is (n x n)
template <>
CVM_API void 
__qle<basic_cmatrix<double, std::complex<double> >, basic_scmatrix<double, std::complex<double> > >
    (const basic_cmatrix<double, std::complex<double> >& mArg, 
    basic_cmatrix<double, std::complex<double> >& mQ,
    basic_scmatrix<double, std::complex<double> >& mL) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint nK = _cvm_min<tint>(nM, nN);

    // we will eventually overwrite mQ to be the output matrix
    mQ = mArg;
    basic_cvector<double, std::complex<double> > vTau(nK);

    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<double> dWork;

    // calculate size of workspace
    ZGEQLF(&nM, &nN, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    ZGEQLF(&nM, &nN, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower-triangular L from overwritten A
    mL.vanish();
    for (tint col = CVM0; col < nN + CVM0; ++col)
        for (tint row = col; row < nK + CVM0; ++row)
            mL(row,col) = mQ(nM - nN + row,col);

    // calculate size of workspace for finding Q
    lWork = -1;
    ZUNGQL(&nM, &nN, &nK, mQ._pd(), mQ._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q
    ZUNGQL(&nM, &nN, &nK, mQ._pd(), mQ._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
}


// QL Case 2: full mode, A is (m x n) and Q is (m x m) and L is (m x n)
template <>
CVM_API void 
__qlf<basic_cmatrix<float, std::complex<float> >, basic_scmatrix<float, std::complex<float> > >
    (const basic_cmatrix<float, std::complex<float> >& mArg, 
    basic_scmatrix<float, std::complex<float> >& mQ,
    basic_cmatrix<float, std::complex<float> >& mL) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();

    // unlike "economy" mode, we need a copy here since Q will be m x m and this may be bigger than original A
    basic_cmatrix<float, std::complex<float> > mA(nM, nM);

    // copy over argument matrix
    mA.assign(CVM0, CVM0, mArg);

    basic_cvector<float, std::complex<float> > vTau(nN);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<float> dWork;

    // calculate size of workspace
    CGEQLF(&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    CGEQLF(&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower triangular L which is now m x n from overwritten A
    mL.vanish();
    for (col = CVM0; col < nN + CVM0; ++col)
        for (row = nM - nN + col; row < nM + CVM0; ++row)
            mL(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is n x n
    lWork = -1;
    CUNGQL(&nM, &nN, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is n x n
    CUNGQL(&nM, &nN, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = CVM0; col < nN + CVM0; ++col)
            mQ(row,nM - nN + col) = mA(row,col);
}


// QL Case 2: full mode, A is (m x n) and Q is (m x m) and L is (m x n)
template <>
CVM_API void 
__qlf<basic_cmatrix<double, std::complex<double> >, basic_scmatrix<double, std::complex<double> > >
    (const basic_cmatrix<double, std::complex<double> >& mArg, 
    basic_scmatrix<double, std::complex<double> >& mQ,
    basic_cmatrix<double, std::complex<double> >& mL) throw(cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();

    // unlike "economy" mode, we need a copy here since Q will be m x m and this may be bigger than original A
    basic_cmatrix<double, std::complex<double> > mA(nM, nM);

    // copy over argument matrix
    mA.assign(CVM0, CVM0, mArg);

    basic_cvector<double, std::complex<double> > vTau(nN);

    tint row, col;
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<double> dWork;

    // calculate size of workspace
    ZGEQLF(&nM, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double> > vWork(lWork);

    // now do most of hardwork, find R and TAU and Householderish bits of Q
    ZGEQLF(&nM, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // get lower triangular L which is now m x n from overwritten A
    mL.vanish();
    for (col = CVM0; col < nN + CVM0; ++col)
        for (row = nM - nN + col; row < nM + CVM0; ++row)
            mL(row,col) = mA(row,col);

    // calculate size of workspace for finding Q that is n x n
    lWork = -1;
    ZUNGQL(&nM, &nN, &nN, mA, mA._pld(), vTau, &dWork, &lWork, &nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    if (lWork > vWork.size()) vWork.resize(lWork);

    // find Q that is n x n
    ZUNGQL(&nM, &nN, &nN, mA, mA._pld(), vTau, vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    for (row = CVM0; row < nM + CVM0; ++row)
        for (col = CVM0; col < nN + CVM0; ++col)
            mQ(row,nM - nN + col) = mA(row,col);
}

// LLS routines
template <>
CVM_API void
__gels<basic_cmatrix<float, std::complex<float> >, basic_cvector<float, std::complex<float> > >
    (bool transpose,
    basic_cmatrix<float, std::complex<float> >& mA,
    const basic_cmatrix<float, std::complex<float> >& mB,
    basic_cmatrix<float, std::complex<float> >& mX,  // output: will be resized anyway, so better pass it empty
    basic_cvector<float, std::complex<float> >& vErr) throw(cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nL = _cvm_min<tint>(nM, nN);
    const tint nK = _cvm_max<tint>(nM, nN);
    const tint nrhs = mB.nsize();
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<float> dWork;
    const char* trans = transpose ? Chars::pC() : Chars::pN();

    mX.resize(nK, nrhs);   // first we might need extra space for residuals
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    CGELS(trans,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          &nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
          &dWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float> > vWork(lWork);

    CGELS(trans,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          &nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
          vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // collecting residuals if applicable
    vErr.set(std::complex<float>(0.F));
    if ((!transpose && nM > nN) || (transpose && nM < nN)) {
        for (tint col = CVM0; col < nrhs + CVM0; col++) {
            for (tint row = nL + CVM0; row < nK + CVM0; row++) {
                vErr[col] += mX(row, col) * mX(row, col);
            }
        }
        mX.resize(nL, nrhs);
    }
}

template <>
CVM_API void
__gels<basic_cmatrix<double, std::complex<double> >, basic_cvector<double, std::complex<double> > >
    (bool transpose,
    basic_cmatrix<double, std::complex<double> >& mA,
    const basic_cmatrix<double, std::complex<double> >& mB,
    basic_cmatrix<double, std::complex<double> >& mX,  // output: will be resized anyway, so better pass it empty
    basic_cvector<double, std::complex<double> >& vErr) throw(cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nL = _cvm_min<tint>(nM, nN);
    const tint nK = _cvm_max<tint>(nM, nN);
    const tint nrhs = mB.nsize();
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<double> dWork;
    const char* trans = transpose ? Chars::pC() : Chars::pN();

    mX.resize(nK, nrhs);   // first we might need extra space for residuals
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    ZGELS(trans,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          &nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
          &dWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double> > vWork(lWork);

    ZGELS(trans,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          &nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
          vWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    // collecting residuals if applicable
    vErr.set(std::complex<double>(0.));
    if ((!transpose && nM > nN) || (transpose && nM < nN)) {
        for (tint col = CVM0; col < nrhs + CVM0; col++) {
            for (tint row = nL + CVM0; row < nK + CVM0; row++) {
                vErr[col] += mX(row, col) * mX(row, col);
            }
        }
        mX.resize(nL, nrhs);
    }
}
//! @endcond

template <>
CVM_API void
__gelsy<float, basic_cmatrix<float, std::complex<float> > >
    (basic_cmatrix<float, std::complex<float> >& mA,
    const basic_cmatrix<float, std::complex<float> >& mB,
    basic_cmatrix<float, std::complex<float> >& mX,  // output: will be resized anyway, so better pass it empty
    float rcond,
    tint& rank) throw(cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nrhs = mB.nsize();
    const tint nK = _cvm_max<tint>(nM, nN);
    tint lWork = -1;
    tint nOutInfo = 0;
    iarray jpvt(nN);
    std::complex<float> dWork;
    basic_rvector<float> rWork(2*nN);

    mX.resize(nK, nrhs);   // we might need extra space here
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    CGELSY(&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
           jpvt, &rcond, &rank, &dWork, &lWork, rWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float> > vWork(lWork);

    CGELSY(&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            jpvt, &rcond, &rank, vWork, &lWork, rWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    mX.resize(nN, nrhs);
}

template <>
CVM_API void
__gelsy<double, basic_cmatrix<double, std::complex<double> > >
    (basic_cmatrix<double, std::complex<double> >& mA,
    const basic_cmatrix<double, std::complex<double> >& mB,
    basic_cmatrix<double, std::complex<double> >& mX,  // output: will be resized anyway, so better pass it empty
    double rcond,
    tint& rank) throw(cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nrhs = mB.nsize();
    const tint nK = _cvm_max<tint>(nM, nN);
    tint lWork = -1;
    tint nOutInfo = 0;
    iarray jpvt(nN);
    std::complex<double> dWork;
    basic_rvector<double> rWork(2*nN);

    mX.resize(nK, nrhs);   // we might need extra space here
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    ZGELSY(&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
           jpvt, &rcond, &rank, &dWork, &lWork, rWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double> > vWork(lWork);

    ZGELSY(&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
            jpvt, &rcond, &rank, vWork, &lWork, rWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    mX.resize(nN, nrhs);
}

//! @cond SPECIALIZATIONS
template <>
CVM_API void
__gelss<float, basic_rvector<float>, basic_cmatrix<float, std::complex<float> > >
    (basic_cmatrix<float, std::complex<float> >& mA,
    const basic_cmatrix<float, std::complex<float> >& mB,
    basic_cmatrix<float, std::complex<float> >& mX,  // output: will be resized anyway, so better pass it empty
    float rcond,
    basic_rvector<float>& vSV, // incr=1 required
    tint& rank) throw(cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nrhs = mB.nsize();
    const tint nK = _cvm_max<tint>(nM, nN);
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<float> dWork;
    basic_rvector<float> rWork(5 * _cvm_min<tint>(nM, nN));

    mX.resize(nK, nrhs);   // we might need extra space here
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    CGELSS(&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
           vSV, &rcond, &rank, &dWork, &lWork, rWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float> > vWork(lWork);

    CGELSS(&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
           vSV, &rcond, &rank, vWork, &lWork, rWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    mX.resize(nN, nrhs);
}

template <>
CVM_API void
__gelss<double, basic_rvector<double>, basic_cmatrix<double, std::complex<double> > >
    (basic_cmatrix<double, std::complex<double> >& mA,
    const basic_cmatrix<double, std::complex<double> >& mB,
    basic_cmatrix<double, std::complex<double> >& mX,  // output: will be resized anyway, so better pass it empty
    double rcond,
    basic_rvector<double>& vSV, // incr=1 required
    tint& rank) throw(cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nrhs = mB.nsize();
    const tint nK = _cvm_max<tint>(nM, nN);
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<double> dWork;
    basic_rvector<double> rWork(5 * _cvm_min<tint>(nM, nN));

    mX.resize(nK, nrhs);   // we might need extra space here
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    ZGELSS(&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
           vSV, &rcond, &rank, &dWork, &lWork, rWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double> > vWork(lWork);

    ZGELSS(&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
           vSV, &rcond, &rank, vWork, &lWork, rWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    mX.resize(nN, nrhs);
}

template <>
CVM_API void
__gelsd<float, basic_rvector<float>, basic_cmatrix<float, std::complex<float> > >
    (basic_cmatrix<float, std::complex<float> >& mA,
    const basic_cmatrix<float, std::complex<float> >& mB,
    basic_cmatrix<float, std::complex<float> >& mX,  // output: will be resized anyway, so better pass it empty
    float rcond,
    basic_rvector<float>& vSV, // incr=1 required
    tint& rank) throw(cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nrhs = mB.nsize();
    const tint nK = _cvm_max<tint>(nM, nN);
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<float> dWork;
    float rWork;
    tint iWork = -1;

    mX.resize(nK, nrhs);   // we might need extra space here
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    CGELSD(&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
           vSV, &rcond, &rank, &dWork, &lWork, &rWork, &iWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float> > vWork(lWork);
    basic_rvector<float> vrWork(static_cast<tint>(rWork));
    basic_array<tint,tint> viWork(iWork);

    CGELSD(&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
           vSV, &rcond, &rank, vWork, &lWork, vrWork, viWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    mX.resize(nN, nrhs);
}

template <>
CVM_API void
__gelsd<double, basic_rvector<double>, basic_cmatrix<double, std::complex<double> > >
    (basic_cmatrix<double, std::complex<double> >& mA,
    const basic_cmatrix<double, std::complex<double> >& mB,
    basic_cmatrix<double, std::complex<double> >& mX,  // output: will be resized anyway, so better pass it empty
    double rcond,
    basic_rvector<double>& vSV, // incr=1 required
    tint& rank) throw(cvmexception)
{
    const tint nM = mA.msize();
    const tint nN = mA.nsize();
    const tint nrhs = mB.nsize();
    const tint nK = _cvm_max<tint>(nM, nN);
    tint lWork = -1;
    tint nOutInfo = 0;
    std::complex<double> dWork;
    double rWork;
    tint iWork = -1;

    mX.resize(nK, nrhs);   // we might need extra space here
    mX.assign(CVM0, CVM0, mB);

    // calculate size of workspace
    ZGELSD(&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
           vSV, &rcond, &rank, &dWork, &lWork, &rWork, &iWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double> > vWork(lWork);
    basic_rvector<double> vrWork(static_cast<tint>(rWork));
    basic_array<tint,tint> viWork(iWork);

    ZGELSD(&nM, &nN, &nrhs, mA._pd(), mA._pld(), mX._pd(), mX._pld(),
           vSV, &rcond, &rank, vWork, &lWork, vrWork, viWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    mX.resize(nN, nrhs);
}
//! @endcond

CVM_NAMESPACE_END

