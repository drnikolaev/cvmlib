//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2014
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "cvm.h"
#include "blas.h"

CVM_NAMESPACE_BEG

template<>
CVM_API float __dot<float> (const float* mpd, tint mnSize, tint mnIncr, const float* pd, tint nIncr)
{
    return SDOT (&mnSize, mpd, &mnIncr, pd, &nIncr);
}

template<>
CVM_API double __dot<double> (const double* mpd, tint mnSize, tint mnIncr, const double* pd, tint nIncr)
{
    return DDOT (&mnSize, mpd, &mnIncr, pd, &nIncr);
}

template<>
CVM_API void
__gemv<float, basic_rmatrix<float>, basic_rvector<float> >
    (bool bLeft,
    const basic_rmatrix<float>& m,
    float dAlpha,
    const basic_rvector<float>& v,
    float dBeta,
    basic_rvector<float>& vRes)
{
    SGEMV (bLeft ? Chars::pT() : Chars::pN(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), m._pn(), &dAlpha, m._pd(), m._pldm(), v, v._pincr(), &dBeta, vRes, vRes._pincr());
}

template<>
CVM_API void
__gemv<double, basic_rmatrix<double>, basic_rvector<double> >
    (bool bLeft,
    const basic_rmatrix<double>& m,
    double dAlpha,
    const basic_rvector<double>& v,
    double dBeta,
    basic_rvector<double>& vRes)
{
    DGEMV (bLeft ? Chars::pT() : Chars::pN(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), m._pn(), &dAlpha, m._pd(), m._pldm(), v, v._pincr(), &dBeta, vRes, vRes._pincr());
}

template<>
CVM_API void
__gbmv<float, basic_srbmatrix<float>, basic_rvector<float> >
    (bool bLeft,
    const basic_srbmatrix<float>& m,
    float dAlpha,
    const basic_rvector<float>& v,
    float dBeta,
    basic_rvector<float>& vRes)
{
    SGBMV (bLeft ? Chars::pT() : Chars::pN(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), m._pn(), m._pl(), m._pu(), &dAlpha, m, m._pld(), v, v._pincr(), &dBeta, vRes, vRes._pincr());
}

template<>
CVM_API void
__gbmv<double, basic_srbmatrix<double>, basic_rvector<double> >
    (bool bLeft,
    const basic_srbmatrix<double>& m,
    double dAlpha,
    const basic_rvector<double>& v,
    double dBeta,
    basic_rvector<double>& vRes)
{
    DGBMV (bLeft ? Chars::pT() : Chars::pN(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), m._pn(), m._pl(), m._pu(), &dAlpha, m, m._pld(), v, v._pincr(), &dBeta, vRes, vRes._pincr());
}

template<>
CVM_API void
__symv<float, basic_srsmatrix<float>, basic_rvector<float> >
    (const basic_srsmatrix<float>& m,
    float dAlpha,
    const basic_rvector<float>& v,
    float dBeta,
    basic_rvector<float>& vRes)
{
    SSYMV (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), &dAlpha, m, m._pld(), v, v._pincr(), &dBeta, vRes, vRes._pincr());
}

template<>
CVM_API void
__symv<double, basic_srsmatrix<double>, basic_rvector<double> >
    (const basic_srsmatrix<double>& m,
    double dAlpha,
    const basic_rvector<double>& v,
    double dBeta,
    basic_rvector<double>& vRes)
{
    DSYMV (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), &dAlpha, m, m._pld(), v, v._pincr(), &dBeta, vRes, vRes._pincr());
}

template<>
CVM_API void
__svd<float, basic_rmatrix<float>, basic_srmatrix<float> >
    (float* pd, tint nSize, tint nIncr, 
    const basic_rmatrix<float>& mArg,
    basic_srmatrix<float>* mU,
    basic_srmatrix<float>* mVH) throw (cvmexception)
{
    const bool bSimple = (mU == nullptr || mVH == nullptr);
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint m = _cvm_min<tint>(nM, nN);
    const tint m4 = m * 4;
    const tint zero(0);
    const tint one(1);
    tint lwork = -1; // to calculate lwork
    tint nOutInfo = 0;

    _check_ne(CVM_SIZESMISMATCH, m, nSize);

    basic_rvector<float> mD (nSize);
    mD.assign (pd, nIncr);

    basic_rvector<float> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_rvector<float> vTauQ (m);
    basic_rvector<float> vTauP (m);
    basic_rmatrix<float> mA (mArg);
    float dwork;

    SGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, &dwork, &lwork, &nOutInfo);
    lwork = static_cast<tint>(dwork);
    basic_rvector<float> vwork (lwork);
    SGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, vwork, &lwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    if (bSimple)
    {
        if (m4 > vwork.size()) vwork.resize(m4);
        SBDSQR (nM >= nN ? Chars::pU() : Chars::pL(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m,
                &zero, &zero, &zero,
                mD, vOffDiag,
                nullptr, &one, nullptr, &one, nullptr, &one,
                vwork, &nOutInfo);
    }
    else
    {
        basic_rmatrix<float> Q (mA);
        basic_rmatrix<float> P (mA);

        if (nM > nN) Q.resize(nM, nM);
        if (nM < nN) P.resize(nN, nN);

        lwork = -1;
        SORGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nM, &nN,
                Q, Q._pld(),
                vTauQ,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lwork = static_cast<tint>(dwork);
        if (lwork > vwork.size()) vwork.resize(lwork);

        SORGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nM, &nN,
                Q, Q._pld(),
                vTauQ,
                vwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        lwork = -1;
        SORGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nN, &nN, &nM,
                P, P._pld(),
                vTauP,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lwork = static_cast<tint>(dwork);
        if (lwork > vwork.size()) vwork.resize(lwork);

        SORGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nN, &nN, &nM,
                P, P._pld(),
                vTauP,
                vwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        if (m4 > vwork.size()) vwork.resize(m4);
        SBDSQR (nM >= nN ? Chars::pU() : Chars::pL(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m,
                &nN, &nM,
                &zero,
                mD, vOffDiag,
                P, P._pld(), Q, Q._pld(), nullptr, &one,
                vwork, &nOutInfo);

        (*mU)  = basic_srmatrix<float>(Q.resize(nM, nM));
        (*mVH) = basic_srmatrix<float>(P.resize(nN, nN));
    }
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "SBDSQR", __FILE__, __LINE__);

    __copy<float> (nSize, mD, mD.incr(), pd, nIncr);
}

template<>
CVM_API void
__svd<double, basic_rmatrix<double>, basic_srmatrix<double> >
    (double* pd, tint nSize, tint nIncr, 
    const basic_rmatrix<double>& mArg,
    basic_srmatrix<double>* mU,
    basic_srmatrix<double>* mVH) throw (cvmexception)
{
    const bool bSimple = (mU == nullptr || mVH == nullptr);
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint m = _cvm_min<tint>(nM, nN);
    const tint m4 = m * 4;
    const tint zero(0);
    const tint one(1);
    tint lwork = -1; // to calculate lwork
    tint nOutInfo = 0;

    _check_ne(CVM_SIZESMISMATCH, m, nSize);

    basic_rvector<double> mD (nSize);
    mD.assign (pd, nIncr);

    basic_rvector<double> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_rvector<double> vTauQ (m);
    basic_rvector<double> vTauP (m);
    basic_rmatrix<double> mA (mArg);
    double dwork;

    DGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, &dwork, &lwork, &nOutInfo);
    lwork = static_cast<tint>(dwork);
    basic_rvector<double> vwork(lwork);
    DGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, vwork, &lwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    if (bSimple)
    {
        if (m4 > vwork.size()) vwork.resize(m4);
        DBDSQR (nM >= nN ? Chars::pU() : Chars::pL(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m,
                &zero, &zero, &zero,
                mD, vOffDiag,
                nullptr, &one, nullptr, &one, nullptr, &one,
                vwork, &nOutInfo);
    }
    else
    {
        basic_rmatrix<double> Q (mA);
        basic_rmatrix<double> P (mA);

        if (nM > nN) Q.resize(nM, nM);
        if (nM < nN) P.resize(nN, nN);

        lwork = -1;
        DORGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nM, &nN,
                Q, Q._pld(),
                vTauQ,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lwork = static_cast<tint>(dwork);
        if (lwork > vwork.size()) vwork.resize(lwork);

        DORGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nM, &nN,
                Q, Q._pld(),
                vTauQ,
                vwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        lwork = -1;
        DORGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nN, &nN, &nM,
                P, P._pld(),
                vTauP,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lwork = static_cast<tint>(dwork);
        if (lwork > vwork.size()) vwork.resize(lwork);

        DORGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nN, &nN, &nM,
                P, P._pld(),
                vTauP,
                vwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        if (m4 > vwork.size()) vwork.resize(m4);
        DBDSQR (nM >= nN ? Chars::pU() : Chars::pL(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m,
                &nN, &nM,
                &zero,
                mD, vOffDiag,
                P, P._pld(), Q, Q._pld(), nullptr, &one,
                vwork, &nOutInfo);

        (*mU)  = basic_srmatrix<double>(Q.resize(nM, nM));
        (*mVH) = basic_srmatrix<double>(P.resize(nN, nN));
    }
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "DBDSQR", __FILE__, __LINE__);

    __copy<double> (nSize, mD, mD.incr(), pd, nIncr);
}

//! @cond SPECIALIZATIONS
template<>
CVM_API void
__svd<float, basic_cmatrix<float, std::complex<float> >, basic_scmatrix<float, std::complex<float> > >
    (float* pd, tint nSize, tint nIncr,
    const basic_cmatrix<float, std::complex<float> >& mArg,
    basic_scmatrix<float, std::complex<float> >* mU,
    basic_scmatrix<float, std::complex<float> >* mVH) throw (cvmexception)
{
    const bool bSimple = (mU == nullptr || mVH == nullptr);
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint m = _cvm_min<tint>(nM, nN);
    const tint zero(0);
    const tint one(1);
    tint lwork = -1; // to calculate lwork
    tint nOutInfo = 0;

    _check_ne(CVM_SIZESMISMATCH, m, nSize);

    basic_rvector<float> mD (nSize);
    mD.assign (pd, nIncr);

    basic_rvector<float> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_cvector<float, std::complex<float> > vTauQ (m);
    basic_cvector<float, std::complex<float> > vTauP (m);
    basic_cmatrix<float, std::complex<float> > mA (mArg);
    basic_rvector<float> rwork (m * 4);
    std::complex<float> dwork;

    CGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, &dwork, &lwork, &nOutInfo);
    lwork = static_cast<tint>(dwork.real());
    basic_cvector<float, std::complex<float> > vwork (lwork);
    CGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, vwork, &lwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    if (bSimple)
    {
        CBDSQR (nM >= nN ? Chars::pU() : Chars::pL(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m,
                &zero, &zero, &zero,
                mD, vOffDiag,
                nullptr, &one, nullptr, &one, nullptr, &one,
                rwork, &nOutInfo);
    }
    else
    {
        basic_cmatrix<float, std::complex<float> > Q (mA);
        basic_cmatrix<float, std::complex<float> > P (mA);

        if (nM > nN) Q.resize(nM, nM);
        if (nM < nN) P.resize(nN, nN);

        lwork = -1;
        CUNGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nM, &nN,
                Q, Q._pld(),
                vTauQ,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lwork = static_cast<tint>(dwork.real());
        if (lwork > vwork.size()) vwork.resize(lwork);

        CUNGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nM, &nN,
                Q, Q._pld(),
                vTauQ,
                vwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        lwork = -1;
        CUNGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nN, &nN, &nM,
                P, P._pld(),
                vTauP,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lwork = static_cast<tint>(dwork.real());
        if (lwork > vwork.size()) vwork.resize(lwork);

        CUNGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nN, &nN, &nM,
                P, P._pld(),
                vTauP,
                vwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        CBDSQR (nM >= nN ? Chars::pU() : Chars::pL(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m,
                &nN, &nM, 
                &zero,
                mD, vOffDiag,
                P, P._pld(), Q, Q._pld(), nullptr, &one,
                rwork, &nOutInfo);

        (*mU)  = basic_scmatrix<float, std::complex<float> >(Q.resize(nM, nM));
        (*mVH) = basic_scmatrix<float, std::complex<float> >(P.resize(nN, nN));
    }
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "CBDSQR", __FILE__, __LINE__);

    __copy<float> (nSize, mD, mD.incr(), pd, nIncr);
}

template<>
CVM_API void
__svd<double, basic_cmatrix<double, std::complex<double> >, basic_scmatrix<double, std::complex<double> > >
    (double* pd, tint nSize, tint nIncr,
    const basic_cmatrix<double, std::complex<double> >& mArg,
    basic_scmatrix<double, std::complex<double> >* mU,
    basic_scmatrix<double, std::complex<double> >* mVH) throw (cvmexception)
{
    const bool bSimple = (mU == nullptr || mVH == nullptr);
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint m = _cvm_min<tint>(nM, nN);
    const tint zero(0);
    const tint one(1);
    tint lwork = -1; // to calculate lwork
    tint nOutInfo = 0;

    _check_ne(CVM_SIZESMISMATCH, m, nSize);

    basic_rvector<double> mD (nSize);
    mD.assign (pd, nIncr);

    basic_rvector<double> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_cvector<double, std::complex<double> > vTauQ (m);
    basic_cvector<double, std::complex<double> > vTauP (m);
    basic_cmatrix<double, std::complex<double> > mA (mArg);
    basic_rvector<double> rwork (m * 4);
    std::complex<double> dwork;

    ZGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, &dwork, &lwork, &nOutInfo);
    lwork = static_cast<tint>(dwork.real());
    basic_cvector<double, std::complex<double> > vwork (lwork);
    ZGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, vwork, &lwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    if (bSimple)
    {
        ZBDSQR (nM >= nN ? Chars::pU() : Chars::pL(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m,
                &zero, &zero, &zero,
                mD, vOffDiag,
                nullptr, &one, nullptr, &one, nullptr, &one,
                rwork, &nOutInfo);
    }
    else
    {
        basic_cmatrix<double, std::complex<double> > Q (mA);
        basic_cmatrix<double, std::complex<double> > P (mA);

        if (nM > nN) Q.resize(nM, nM);
        if (nM < nN) P.resize(nN, nN);

        lwork = -1;
        ZUNGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nM, &nN,
                Q, Q._pld(),
                vTauQ,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lwork = static_cast<tint>(dwork.real());
        if (lwork > vwork.size()) vwork.resize(lwork);

        ZUNGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nM, &nN,
                Q, Q._pld(),
                vTauQ,
                vwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        lwork = -1;
        ZUNGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nN, &nN, &nM,
                P, P._pld(),
                vTauP,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lwork = static_cast<tint>(dwork.real());
        if (lwork > vwork.size()) vwork.resize(lwork);

        ZUNGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nN, &nN, &nM,
                P, P._pld(),
                vTauP,
                vwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        ZBDSQR (nM >= nN ? Chars::pU() : Chars::pL(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m,
                &nN, &nM, 
                &zero,
                mD, vOffDiag,
                P, P._pld(), Q, Q._pld(), nullptr, &one,
                rwork, &nOutInfo);

        (*mU)  = basic_scmatrix<double, std::complex<double> >(Q.resize(nM, nM));
        (*mVH) = basic_scmatrix<double, std::complex<double> >(P.resize(nN, nN));
    }
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "ZBDSQR", __FILE__, __LINE__);

    __copy<double> (nSize, mD, mD.incr(), pd, nIncr);
}
//! @endcond

template<>
CVM_API void
__svd<float, basic_srbmatrix<float>, basic_srmatrix<float> >
    (float* pd, tint nSize, tint nIncr,
    const basic_srbmatrix<float>& mArg,
    basic_srmatrix<float>* mU,
    basic_srmatrix<float>* mVH) throw (cvmexception)
{
    const tint zero(0);
    const tint m = mArg.msize();

    _check_ne(CVM_SIZESMISMATCH, m, nSize);

    const bool bSimple = (mU == nullptr || mVH == nullptr);
    tint nOutInfo = 0;

    basic_rvector<float> mD (nSize);
    mD.assign (pd, nIncr);

    basic_rvector  <float> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_srmatrix <float> mQ       (bSimple ? 1 : m);
    basic_srmatrix <float> mPT      (bSimple ? 1 : m);
    basic_srmatrix <float> mC       (1);
    basic_rvector  <float> vwork    (m * 4); // 2*max(M,N), but M==N, then sharing it with 4*N
    basic_srbmatrix<float> mA       (mArg);

    SGBBRD (bSimple ? Chars::pN() : Chars::pB(), 
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            mA._pm(), mA._pn(), &zero, mA._pl(), mA._pu(), mA, mA._pld(),
            mD, vOffDiag,
            mQ, mQ._pm(),
            mPT, mPT._pm(),
            mC, mC._pm(), 
            vwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    SBDSQR (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &m,
            bSimple ? &zero : &m,
            bSimple ? &zero : &m,
            &zero,
            mD, vOffDiag,
            mPT, mPT._pm(),
            mQ, mQ._pm(),
            mC, mC._pm(), 
            vwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "SBDSQR", __FILE__, __LINE__);

    if (!bSimple)
    {
        (*mU)  = mQ;
        (*mVH) = mPT;
    }

    __copy<float> (nSize, mD, mD.incr(), pd, nIncr);
}

template<>
CVM_API void
__svd<double, basic_srbmatrix<double>, basic_srmatrix<double> >
    (double* pd, tint nSize, tint nIncr,
    const basic_srbmatrix<double>& mArg,
    basic_srmatrix<double>* mU,
    basic_srmatrix<double>* mVH) throw (cvmexception)
{
    const tint zero(0);
    const tint m = mArg.msize();

    _check_ne(CVM_SIZESMISMATCH, m, nSize);

    const bool bSimple = (mU == nullptr || mVH == nullptr);
    tint nOutInfo = 0;

    basic_rvector<double> mD (nSize);
    mD.assign (pd, nIncr);

    basic_rvector  <double> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_srmatrix <double> mQ       (bSimple ? 1 : m);
    basic_srmatrix <double> mPT      (bSimple ? 1 : m);
    basic_srmatrix <double> mC       (1);
    basic_rvector  <double> vwork    (m * 4); // 2*max(M,N), but M==N, so sharing it with 4*N
    basic_srbmatrix<double> mA       (mArg);

    DGBBRD (bSimple ? Chars::pN() : Chars::pB(), 
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            mA._pm(), mA._pn(), &zero, mA._pl(), mA._pu(), mA, mA._pld(),
            mD, vOffDiag,
            mQ, mQ._pm(),
            mPT, mPT._pm(),
            mC, mC._pm(), 
            vwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    DBDSQR (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &m,
            bSimple ? &zero : &m,
            bSimple ? &zero : &m,
            &zero,
            mD, vOffDiag,
            mPT, mPT._pm(),
            mQ, mQ._pm(),
            mC, mC._pm(), 
            vwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "DBDSQR", __FILE__, __LINE__);

    if (!bSimple)
    {
        (*mU)  = mQ;
        (*mVH) = mPT;
    }

    __copy<double> (nSize, mD, mD.incr(), pd, nIncr);
}

//! @cond SPECIALIZATIONS
template<>
CVM_API void
__svd<float, basic_scbmatrix<float, std::complex<float> >, basic_scmatrix<float, std::complex<float> > >
    (float* pd, tint nSize, tint nIncr,
    const basic_scbmatrix<float, std::complex<float> >& mArg,
    basic_scmatrix<float, std::complex<float> >* mU,
    basic_scmatrix<float, std::complex<float> >* mVH) throw (cvmexception)
{
    const tint zero(0);
    const tint m = mArg.msize();

    _check_ne(CVM_SIZESMISMATCH, m, nSize);

    const bool bSimple = (mU == nullptr || mVH == nullptr);
    tint nOutInfo = 0;

    basic_rvector<float> mD (nSize);
    mD.assign (pd, nIncr);

    basic_rvector  <float> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_scmatrix <float, std::complex<float> > mQ (bSimple ? 1 : m);
    basic_scmatrix <float, std::complex<float> > mPT (bSimple ? 1 : m);
    basic_scmatrix <float, std::complex<float> > mC (1);
    basic_cvector  <float, std::complex<float> > vwork (m);
    basic_scbmatrix<float, std::complex<float> > mA (mArg);
    basic_rvector  <float> vrwork (m * 4); // max(M,N), but M==N, so sharing it with 4*N

    CGBBRD (bSimple ? Chars::pN() : Chars::pB(), 
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            mA._pm(), mA._pn(), &zero, mA._pl(), mA._pu(), mA, mA._pld(),
            mD, vOffDiag,
            mQ, mQ._pm(),
            mPT, mPT._pm(),
            mC, mC._pm(), 
            vwork, vrwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    CBDSQR (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &m,
            bSimple ? &zero : &m,
            bSimple ? &zero : &m,
            &zero,
            mD, vOffDiag,
            mPT, mPT._pm(),
            mQ, mQ._pm(),
            mC, mC._pm(), 
            vrwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "CBDSQR", __FILE__, __LINE__);

    if (!bSimple)
    {
        (*mU)  = mQ;
        (*mVH) = mPT;
    }

    __copy<float> (nSize, mD, mD.incr(), pd, nIncr);
}

template<>
CVM_API void
__svd<double, basic_scbmatrix<double, std::complex<double> >, basic_scmatrix<double, std::complex<double> > >
    (double* pd, tint nSize, tint nIncr,
    const basic_scbmatrix<double, std::complex<double> >& mArg,
    basic_scmatrix<double, std::complex<double> >* mU,
    basic_scmatrix<double, std::complex<double> >* mVH) throw (cvmexception)
{
    const tint zero(0);
    const tint m = mArg.msize();

    _check_ne(CVM_SIZESMISMATCH, m, nSize);

    const bool bSimple = (mU == nullptr || mVH == nullptr);
    tint nOutInfo = 0;

    basic_rvector<double> mD (nSize);
    mD.assign (pd, nIncr);

    basic_rvector  <double> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_scmatrix <double, std::complex<double> > mQ (bSimple ? 1 : m);
    basic_scmatrix <double, std::complex<double> > mPT (bSimple ? 1 : m);
    basic_scmatrix <double, std::complex<double> > mC (1);
    basic_cvector  <double, std::complex<double> > vwork (m);
    basic_scbmatrix<double, std::complex<double> > mA (mArg);
    basic_rvector  <double> vrwork (m * 4); // max(M,N), but M==N, so sharing it with 4*N

    ZGBBRD (bSimple ? Chars::pN() : Chars::pB(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            mA._pm(), mA._pn(), &zero, mA._pl(), mA._pu(), mA, mA._pld(),
            mD, vOffDiag,
            mQ, mQ._pm(),
            mPT, mPT._pm(),
            mC, mC._pm(),
            vwork, vrwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    basic_rvector<double> vwork2 (m * 4);
    ZBDSQR (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &m,
            bSimple ? &zero : &m,
            bSimple ? &zero : &m,
            &zero,
            mD, vOffDiag,
            mPT, mPT._pm(),
            mQ, mQ._pm(),
            mC, mC._pm(),
            vrwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "ZBDSQR", __FILE__, __LINE__);

    if (!bSimple)
    {
        (*mU)  = mQ;
        (*mVH) = mPT;
    }

    __copy<double> (nSize, mD, mD.incr(), pd, nIncr);
}
//! @endcond

template<>
CVM_API void
__eig<basic_rvector<float>, basic_srsmatrix<float>, basic_srmatrix<float> >
    (basic_rvector<float>& vRes,
    const basic_srsmatrix<float>& mArg,
    basic_srmatrix<float>* mEigVect,
    bool /*bRightVect*/) throw (cvmexception)
{
    const tint nM = mArg.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, vRes.size());
    const bool bEigVect = (mEigVect != nullptr);

    if (nM == 1)
    {
        vRes[CVM0] = mArg(CVM0,CVM0);
        if (bEigVect)
        {
            const float one(1.F);
            mEigVect -> resize (1);
            (*mEigVect)[CVM0].set (one);
        }
    }
    else
    {
        const char* pcJob = bEigVect ? Chars::pV() : Chars::pN();
        basic_srsmatrix<float> mA (mArg);
        tint lwork = -1, liwork = -1;
        float work_size = 0.;
        tint iwork_size = 0;
        tint nOutInfo = 0;

        SSYEVD (pcJob,
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                Chars::pU(),
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                &nM, mA, mA._pld(), vRes, &work_size, &lwork, &iwork_size, &liwork, &nOutInfo);
        lwork  = static_cast<tint> (work_size);
        liwork = iwork_size;
        basic_rvector<float> work (lwork);
        basic_array<tint,tint> iwork (liwork);

        SSYEVD (pcJob,
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                Chars::pU(),
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                &nM, mA, mA._pld(), vRes, work, &lwork, iwork, &liwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "SSYEVD", __FILE__, __LINE__);

        if (bEigVect)
        {
            (*mEigVect) << mA;
        }
    }
}

template<>
CVM_API void
__eig<basic_rvector<double>, basic_srsmatrix<double>, basic_srmatrix<double> >
    (basic_rvector<double>& vRes,
    const basic_srsmatrix<double>& mArg,
    basic_srmatrix<double>* mEigVect,
    bool /*bRightVect*/) throw (cvmexception)
{
    const tint nM = mArg.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, vRes.size());
    const bool bEigVect = (mEigVect != nullptr);

    if (nM == 1)
    {
        vRes[CVM0] = mArg(CVM0,CVM0);
        if (bEigVect)
        {
            const double one(1.);
            mEigVect -> resize (1);
            (*mEigVect)[CVM0].set (one);
        }
    }
    else
    {
        const char* pcJob = bEigVect ? Chars::pV() : Chars::pN();
        basic_srsmatrix<double> mA (mArg);
        tint lwork = -1, liwork = -1;
        double work_size = 0.;
        tint iwork_size = 0;
        tint nOutInfo = 0;

        DSYEVD (pcJob,
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                Chars::pU(),
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                &nM, mA, mA._pld(), vRes, &work_size, &lwork, &iwork_size, &liwork, &nOutInfo);
        lwork  = static_cast<tint> (work_size);
        liwork = iwork_size;
        basic_rvector<double> work (lwork);
        basic_array<tint,tint> iwork (liwork);

        DSYEVD (pcJob,
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                Chars::pU(),
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                &nM, mA, mA._pld(), vRes, work, &lwork, iwork, &liwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "DSYEVD", __FILE__, __LINE__);

        if (bEigVect)
        {
            (*mEigVect) << mA;
        }
    }
}

//! @cond SPECIALIZATIONS
template<>
CVM_API void
__eig<basic_rvector<float>, basic_schmatrix<float, std::complex<float> >, basic_scmatrix<float, std::complex<float> > >
    (basic_rvector<float>& vRes,
    const basic_schmatrix<float, std::complex<float> >& mArg,
    basic_scmatrix<float, std::complex<float> >* mEigVect,
    bool /*bRightVect*/) throw (cvmexception)
{
    const tint nM = mArg.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, vRes.size());
    const bool bEigVect = (mEigVect != nullptr);

    if (nM == 1)
    {
        vRes[CVM0] = 1.F;
        if (bEigVect)
        {
            mEigVect -> resize (1);
            (*mEigVect)[CVM0].set (mArg(CVM0,CVM0));
        }
    }
    else
    {
        const char* pcJob = bEigVect ? Chars::pV() : Chars::pN();
        basic_schmatrix<float, std::complex<float> > mA (mArg);
        tint lwork = -1, lrwork = -1, liwork = -1;
        std::complex<float> work_size;
        float rwork_size = 0.;
        tint iwork_size = 0;
        tint nOutInfo = 0;

        CHEEVD (pcJob,
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                Chars::pU(),
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                &nM, mA, mA._pld(), vRes, &work_size, &lwork, &rwork_size, &lrwork, &iwork_size, &liwork, &nOutInfo);
        lwork  = static_cast<tint> (work_size.real());
        lrwork = static_cast<tint> (rwork_size);
        liwork = iwork_size;
        basic_cvector<float, std::complex<float> > work (lwork);
        basic_rvector<float> rwork (lrwork);
        basic_array<tint,tint> iwork (liwork);

        CHEEVD (pcJob,
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                Chars::pU(),
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                &nM, mA, mA._pld(), vRes, work, &lwork, rwork, &lrwork, iwork, &liwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "CHEEVD", __FILE__, __LINE__);

        if (bEigVect)
        {
            (*mEigVect) << mA;
        }
    }
}

template<>
CVM_API void
__eig<basic_rvector<double>, basic_schmatrix<double, std::complex<double> >, basic_scmatrix<double, std::complex<double> > >
    (basic_rvector<double>& vRes,
    const basic_schmatrix<double, std::complex<double> >& mArg,
    basic_scmatrix<double, std::complex<double> >* mEigVect,
    bool /*bRightVect*/) throw (cvmexception)
{
    const tint nM = mArg.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, vRes.size());
    const bool bEigVect = (mEigVect != nullptr);

    if (nM == 1)
    {
        vRes[CVM0] = 1.;
        if (bEigVect)
        {
            mEigVect -> resize (1);
            (*mEigVect)[CVM0].set (mArg(CVM0,CVM0));
        }
    }
    else
    {
        const char* pcJob = bEigVect ? Chars::pV() : Chars::pN();
        basic_schmatrix<double, std::complex<double> > mA (mArg);
        tint lwork = -1, lrwork = -1, liwork = -1;
        std::complex<double> work_size;
        double rwork_size = 0.;
        tint iwork_size = 0;
        tint nOutInfo = 0;

        ZHEEVD (pcJob,
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                Chars::pU(),
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                &nM, mA, mA._pld(), vRes, &work_size, &lwork, &rwork_size, &lrwork, &iwork_size, &liwork, &nOutInfo);
        lwork  = static_cast<tint> (work_size.real());
        lrwork = static_cast<tint> (rwork_size);
        liwork = iwork_size;
        basic_cvector<double, std::complex<double> > work (lwork);
        basic_rvector<double> rwork (lrwork);
        basic_array<tint,tint> iwork (liwork);

        ZHEEVD (pcJob,
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                Chars::pU(),
    #if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
    #endif
                &nM, mA, mA._pld(), vRes, work, &lwork, rwork, &lrwork, iwork, &liwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "ZHEEVD", __FILE__, __LINE__);

        if (bEigVect)
        {
            (*mEigVect) << mA;
        }
    }
}
//! @endcond

// pseudo (generalized) inversion routines
template<>
CVM_API void
__pinv<float, basic_rmatrix<float>, basic_rmatrix<float> >
    (basic_rmatrix<float>& mX,
    const basic_rmatrix<float>& mArg, float threshold) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint m  = _cvm_min<tint>(nM, nN);
    tint lwork    = -1; // to calculate lwork
    tint nOutInfo = 0;

    basic_rvector<float> mD       (m);
    basic_rvector<float> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_rvector<float> vTauQ    (m);
    basic_rvector<float> vTauP    (m);
    basic_rmatrix<float> mA       (mArg);
    float dwork;

    SGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, &dwork, &lwork, &nOutInfo);
    lwork = static_cast<tint>(dwork);
    basic_rvector<float> vwork(lwork);
    SGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, vwork, &lwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    const tint zero(0);
    const tint one(1);
    
    // few words about economy:
    // for m > n case we care about m-by-n matrix U and n-by-n matrix VH
    // for m < n case we care about m-by-m matrix U and m-by-n matrix VH
    // however, the whole matrix A is needed to start computations
    basic_rmatrix<float> Q (mA);
    basic_rmatrix<float> P (mA);

    {
        lwork = -1;
        SORGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &m, &nN,
                Q, Q._pld(),
                vTauQ,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        lwork = static_cast<tint>(dwork);
        basic_rvector<float> vwork3(lwork);
        SORGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &m, &nN,
                Q, Q._pld(),
                vTauQ,
                vwork3, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }

    {
        lwork = -1;
        SORGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m, &nN, &nM,
                P, P._pld(),
                vTauP,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        lwork = static_cast<tint>(dwork);
        basic_rvector<float> vwork3(lwork);
        SORGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m, &nN, &nM,
                P, P._pld(),
                vTauP,
                vwork3, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }

    basic_rvector<float> vwork2 (m * 4);
    SBDSQR (nM >= nN ? Chars::pU() : Chars::pL(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &m,
            &nN, &nM,
            &zero,
            mD, vOffDiag,
            P, P._pld(), Q, Q._pld(), nullptr, &one,
            vwork2, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "SBDSQR", __FILE__, __LINE__);

    if (nM > nN) P.resize(nN, nN);   // VH
    if (nM < nN) Q.resize(nM, nM);   // U
    for (tint i = CVM0; i < P.msize() + CVM0; ++i) {
        if (mD[i] > threshold) {
            P[i] /= mD[i];  // multiplying V by S^-1
        }
        else {
            P[i].set(0.F);
        }
    }
    mX.mult (~P, ~Q);
}

template<>
CVM_API void
__pinv<double, basic_rmatrix<double>, basic_rmatrix<double> >
    (basic_rmatrix<double>& mX,
    const basic_rmatrix<double>& mArg, double threshold) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint m  = _cvm_min<tint>(nM, nN);
    tint lwork    = -1; // to calculate lwork
    tint nOutInfo = 0;

    basic_rvector<double> mD       (m);
    basic_rvector<double> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_rvector<double> vTauQ    (m);
    basic_rvector<double> vTauP    (m);
    basic_rmatrix<double> mA       (mArg);
    double dwork;

    DGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, &dwork, &lwork, &nOutInfo);
    lwork = static_cast<tint>(dwork);
    basic_rvector<double> vwork(lwork);
    DGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, vwork, &lwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    const tint zero(0);
    const tint one(1);
    
    // few words about economy:
    // for m > n case we care about m-by-n matrix U and n-by-n matrix VH
    // for m < n case we care about m-by-m matrix U and m-by-n matrix VH
    // however, the whole matrix A is needed to start computations
    basic_rmatrix<double> Q (mA);
    basic_rmatrix<double> P (mA);

    {
        lwork = -1;
        DORGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &m, &nN,
                Q, Q._pld(),
                vTauQ,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
            

        lwork = static_cast<tint>(dwork);
        basic_rvector<double> vwork3(lwork);
        DORGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &m, &nN,
                Q, Q._pld(),
                vTauQ,
                vwork3, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }

    {
        lwork = -1;
        DORGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m, &nN, &nM,
                P, P._pld(),
                vTauP,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        lwork = static_cast<tint>(dwork);
        basic_rvector<double> vwork3(lwork);
        DORGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m, &nN, &nM,
                P, P._pld(),
                vTauP,
                vwork3, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }

    basic_rvector<double> vwork2 (m * 4);
    DBDSQR (nM >= nN ? Chars::pU() : Chars::pL(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &m,
            &nN, &nM,
            &zero,
            mD, vOffDiag,
            P, P._pld(), Q, Q._pld(), nullptr, &one,
            vwork2, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "DBDSQR", __FILE__, __LINE__);

    if (nM > nN) P.resize(nN, nN);   // VH
    if (nM < nN) Q.resize(nM, nM);   // U
    for (tint i = CVM0; i < P.msize() + CVM0; ++i) {
        if (mD[i] > threshold) {
            P[i] /= mD[i];  // multiplying V by S^-1
        }
        else {
            P[i].set(0.);
        }
    }
    mX.mult (~P, ~Q);
}

//! @cond SPECIALIZATIONS
template<>
CVM_API void
__pinv<float, basic_cmatrix<float, std::complex<float> >, basic_cmatrix<float, std::complex<float> > >
    (basic_cmatrix<float, std::complex<float> >& mX, 
    const basic_cmatrix<float, std::complex<float> >& mArg, float threshold) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint m  = _cvm_min<tint>(nM, nN);
    tint lwork    = -1; // to calculate lwork
    tint nOutInfo = 0;

    basic_rvector<float> mD (m);
    basic_rvector<float> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_cvector<float, std::complex<float> > vTauQ (m);
    basic_cvector<float, std::complex<float> > vTauP (m);
    basic_cmatrix<float, std::complex<float> > mA (mArg);
    std::complex<float> dwork;

    CGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, &dwork, &lwork, &nOutInfo);
    lwork = static_cast<tint>(dwork.real());
    basic_cvector<float, std::complex<float> > vwork(lwork);
    CGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, vwork, &lwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    const tint zero(0);
    const tint one(1);
    
    // few words about economy:
    // for m > n case we care about m-by-n matrix U and n-by-n matrix VH
    // for m < n case we care about m-by-m matrix U and m-by-n matrix VH
    // however, the whole matrix A is needed to start computations
    basic_cmatrix<float, std::complex<float> > Q (mA);
    basic_cmatrix<float, std::complex<float> > P (mA);

    {
        lwork = -1;
        CUNGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &m, &nN,
                Q, Q._pld(),
                vTauQ,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        lwork = static_cast<tint>(dwork.real());
        basic_cvector<float, std::complex<float> > vwork3 (lwork);
        CUNGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &m, &nN,
                Q, Q._pld(),
                vTauQ,
                vwork3, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }

    {
        lwork = -1;
        CUNGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m, &nN, &nM,
                P, P._pld(),
                vTauP,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    
        lwork = static_cast<tint>(dwork.real());
        basic_cvector<float, std::complex<float> > vwork3 (lwork);
        CUNGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m, &nN, &nM,
                P, P._pld(),
                vTauP,
                vwork3, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }

    basic_rvector<float> vwork2 (m * 4);
    CBDSQR (nM >= nN ? Chars::pU() : Chars::pL(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &m,
            &nN, &nM,
            &zero,
            mD, vOffDiag,
            P, P._pld(), Q, Q._pld(), nullptr, &one,
            vwork2, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "CBDSQR", __FILE__, __LINE__);

    if (nM > nN) P.resize(nN, nN);   // VH
    if (nM < nN) Q.resize(nM, nM);   // U
    for (tint i = CVM0; i < P.msize() + CVM0; ++i) {
        if (mD[i] > threshold) {
            P[i] /= mD[i];  // multiplying V by S^-1
        }
        else {
            P[i].set(0.F);
        }
    }
    mX.mult (~P, ~Q);
}

template<>
CVM_API void
__pinv<double, basic_cmatrix<double, std::complex<double> >, basic_cmatrix<double, std::complex<double> > >
    (basic_cmatrix<double, std::complex<double> >& mX,
    const basic_cmatrix<double, std::complex<double> >& mArg, double threshold) throw (cvmexception)
{
    const tint nM = mArg.msize();
    const tint nN = mArg.nsize();
    const tint m  = _cvm_min<tint>(nM, nN);
    tint lwork    = -1; // to calculate lwork
    tint nOutInfo = 0;

    basic_rvector<double> mD (m);
    basic_rvector<double> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_cvector<double, std::complex<double> > vTauQ (m);
    basic_cvector<double, std::complex<double> > vTauP (m);
    basic_cmatrix<double, std::complex<double> > mA (mArg);
    std::complex<double> dwork;

    ZGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, &dwork, &lwork, &nOutInfo);
    lwork = static_cast<tint>(dwork.real());
    basic_cvector<double, std::complex<double> > vwork(lwork);
    ZGEBRD (&nM, &nN, mA, mA._pld(), mD, vOffDiag, vTauQ, vTauP, vwork, &lwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    const tint zero(0);
    const tint one(1);
    
    // few words about economy:
    // for m > n case we care about m-by-n matrix U and n-by-n matrix VH
    // for m < n case we care about m-by-m matrix U and m-by-n matrix VH
    // however, the whole matrix A is needed to start computations
    basic_cmatrix<double, std::complex<double> > Q (mA);
    basic_cmatrix<double, std::complex<double> > P (mA);

    {
        lwork = -1;
        ZUNGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &m, &nN,
                Q, Q._pld(),
                vTauQ,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    
        lwork = static_cast<tint>(dwork.real());
        basic_cvector<double, std::complex<double> > vwork3 (lwork);
        ZUNGBR (Chars::pQ(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &m, &nN,
                Q, Q._pld(),
                vTauQ,
                vwork3, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }

    {
        lwork = -1;
        ZUNGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m, &nN, &nM,
                P, P._pld(),
                vTauP,
                &dwork, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        lwork = static_cast<tint>(dwork.real());
        basic_cvector<double, std::complex<double> > vwork3 (lwork);
        ZUNGBR (Chars::pP(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &m, &nN, &nM,
                P, P._pld(),
                vTauP,
                vwork3, &lwork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }

    basic_rvector<double> vwork2 (m * 4);
    ZBDSQR (nM >= nN ? Chars::pU() : Chars::pL(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &m,
            &nN, &nM,
            &zero,
            mD, vOffDiag,
            P, P._pld(), Q, Q._pld(), nullptr, &one,
            vwork2, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "ZBDSQR", __FILE__, __LINE__);

    if (nM > nN) P.resize(nN, nN);   // VH
    if (nM < nN) Q.resize(nM, nM);   // U
    for (tint i = CVM0; i < P.msize() + CVM0; ++i) {
        if (mD[i] > threshold) {
            P[i] /= mD[i];  // multiplying V by S^-1
        }
        else {
            P[i].set(0.);
        }
    }
    mX.mult (~P, ~Q);
}
//! @endcond

template<>
CVM_API void
__pinv<float, basic_srbmatrix<float>, basic_rmatrix<float> >
    (basic_rmatrix<float>& mX, 
    const basic_srbmatrix<float>& mArg, float threshold) throw (cvmexception)
{
    const tint zero(0);
    const tint m = mArg.msize();
    tint nOutInfo = 0;

    basic_rvector  <float> mD       (m);
    basic_rvector  <float> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_srmatrix <float> mQ       (m);
    basic_srmatrix <float> mPT      (m);
    basic_srmatrix <float> mC       (1);
    basic_rvector  <float> vwork    (2 * m);
    basic_srbmatrix<float> mA       (mArg);

    SGBBRD (Chars::pB(), 
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            mA._pm(), mA._pn(), &zero, mA._pl(), mA._pu(), mA, mA._pld(),
            mD, vOffDiag,
            mQ, mQ._pm(),
            mPT, mPT._pm(),
            mC, mC._pm(), 
            vwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    basic_rvector<float> vwork2 (m * 4);
    SBDSQR (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &m,
            &m,
            &m,
            &zero,
            mD, vOffDiag,
            mPT, mPT._pm(),
            mQ, mQ._pm(),
            mC, mC._pm(), 
            vwork2, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "SBDSQR", __FILE__, __LINE__);

    for (tint i = CVM0; i < m + CVM0; ++i) {
        if (mD[i] > threshold) {
            mPT[i] /= mD[i];  // multiplying V by S^-1
        }
        else {
            mPT[i].set(0.F);
        }
    }
    mX.mult (~mPT, ~mQ);
}

template<>
CVM_API void
__pinv<double, basic_srbmatrix<double>, basic_rmatrix<double> >
    (basic_rmatrix<double>& mX, 
    const basic_srbmatrix<double>& mArg, double threshold) throw (cvmexception)
{
    const tint zero(0);
    const tint m = mArg.msize();
    tint nOutInfo = 0;

    basic_rvector  <double> mD       (m);
    basic_rvector  <double> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_srmatrix <double> mQ       (m);
    basic_srmatrix <double> mPT      (m);
    basic_srmatrix <double> mC       (1);
    basic_rvector  <double> vwork    (2 * m);
    basic_srbmatrix<double> mA       (mArg);

    DGBBRD (Chars::pB(), 
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            mA._pm(), mA._pn(), &zero, mA._pl(), mA._pu(), mA, mA._pld(),
            mD, vOffDiag,
            mQ, mQ._pm(),
            mPT, mPT._pm(),
            mC, mC._pm(), 
            vwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    basic_rvector<double> vwork2 (m * 4);
    DBDSQR (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &m,
            &m,
            &m,
            &zero,
            mD, vOffDiag,
            mPT, mPT._pm(),
            mQ, mQ._pm(),
            mC, mC._pm(), 
            vwork2, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "DBDSQR", __FILE__, __LINE__);

    for (tint i = CVM0; i < m + CVM0; ++i) {
        if (mD[i] > threshold) {
            mPT[i] /= mD[i];  // multiplying V by S^-1
        }
        else {
            mPT[i].set(0.);
        }
    }
    mX.mult (~mPT, ~mQ);
}


//! @cond SPECIALIZATIONS
template<>
CVM_API void
__pinv<float, basic_scbmatrix<float, std::complex<float> >, basic_cmatrix<float, std::complex<float> > >
    (basic_cmatrix<float, std::complex<float> >& mX, 
    const basic_scbmatrix<float, std::complex<float> >& mArg, float threshold) throw (cvmexception)
{
    const tint zero(0);
    const tint m = mArg.msize();
    tint nOutInfo = 0;

    basic_rvector  <float> mD (m);
    basic_rvector  <float> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_scmatrix <float, std::complex<float> > mQ (m);
    basic_scmatrix <float, std::complex<float> > mPT (m);
    basic_scmatrix <float, std::complex<float> > mC (1);
    basic_cvector  <float, std::complex<float> > vwork (2 * m);
    basic_scbmatrix<float, std::complex<float> > mA (mArg);
    basic_rvector  <float> vRwork (m);

    CGBBRD (Chars::pB(), 
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            mA._pm(), mA._pn(), &zero, mA._pl(), mA._pu(), mA, mA._pld(),
            mD, vOffDiag,
            mQ, mQ._pm(),
            mPT, mPT._pm(),
            mC, mC._pm(), 
            vwork, vRwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    basic_rvector<float> vwork2 (m * 4);
    CBDSQR (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &m,
            &m,
            &m,
            &zero,
            mD, vOffDiag,
            mPT, mPT._pm(),
            mQ, mQ._pm(),
            mC, mC._pm(), 
            vwork2, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "CBDSQR", __FILE__, __LINE__);

    for (tint i = CVM0; i < m + CVM0; ++i) {
        if (mD[i] > threshold) {
            mPT[i] /= mD[i];  // multiplying V by S^-1
        }
        else {
            mPT[i].set(0.F);
        }
    }
    mX.mult (~mPT, ~mQ);
}

template<>
CVM_API void
__pinv<double, basic_scbmatrix<double, std::complex<double> >, basic_cmatrix<double, std::complex<double> > >
    (basic_cmatrix<double, std::complex<double> >& mX, 
    const basic_scbmatrix<double, std::complex<double> >& mArg, double threshold) throw (cvmexception)
{
    const tint zero(0);
    const tint m = mArg.msize();
    tint nOutInfo = 0;

    basic_rvector  <double> mD (m);
    basic_rvector  <double> vOffDiag (_cvm_max<tint>(1, m - 1));
    basic_scmatrix <double, std::complex<double> > mQ (m);
    basic_scmatrix <double, std::complex<double> > mPT (m);
    basic_scmatrix <double, std::complex<double> > mC (1);
    basic_cvector  <double, std::complex<double> > vwork (2 * m);
    basic_rvector  <double> vRwork (m);
    basic_scbmatrix<double, std::complex<double> > mA (mArg);

    ZGBBRD (Chars::pB(), 
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            mA._pm(), mA._pn(), &zero, mA._pl(), mA._pu(), mA, mA._pld(),
            mD, vOffDiag,
            mQ, mQ._pm(),
            mPT, mPT._pm(),
            mC, mC._pm(), 
            vwork, vRwork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    basic_rvector<double> vwork2 (m * 4);
    ZBDSQR (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &m,
            &m,
            &m,
            &zero,
            mD, vOffDiag,
            mPT, mPT._pm(),
            mQ, mQ._pm(),
            mC, mC._pm(), 
            vwork2, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "ZBDSQR", __FILE__, __LINE__);

    for (tint i = CVM0; i < m + CVM0; ++i) {
        if (mD[i] > threshold) {
            mPT[i] /= mD[i];  // multiplying V by S^-1
        }
        else {
            mPT[i].set(0.);
        }
    }
    mX.mult (~mPT, ~mQ);
}
//! @endcond

CVM_NAMESPACE_END

