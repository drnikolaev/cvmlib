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
__exp<basic_scmatrix<float, std::complex<float> >, float>
    (basic_scmatrix<float, std::complex<float> >& m,
    const basic_scmatrix<float, std::complex<float> >& mArg,
    float tol) throw(cvmexception)
{
    tint nR = 0, nI = 0, nQ = 0, nJ = 0;
    const tint mnM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, mnM, mArg.msize());

    basic_scmatrix<float, std::complex<float> > mTmp;
    const std::complex<float>* pd = mArg._pd();

    if (pd == m.get())
    {
        mTmp << mArg;
        pd = mTmp;
    }

    CMEXPC(&mnM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), &tol, &nR, &nI, &nQ, &nJ);
    basic_cvector<float, std::complex<float> > vR(nR);
    basic_array<tint,tint> vI(nI);

    const tint issymm = 0;
    std::complex<float> work_dummy(0.F);
    const tint lwork_dummy = 0;

    CMEXP(&mnM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), m, m._pld(),
           vR, vI, &nR, &nI, &nQ, &nJ, &issymm, &work_dummy, &lwork_dummy);
}

template<>
CVM_API void
__exp<basic_scmatrix<double, std::complex<double> >, double>
    (basic_scmatrix<double, std::complex<double> >& m,
    const basic_scmatrix<double, std::complex<double> >& mArg,
    double tol) throw(cvmexception)
{
    tint nR = 0, nI = 0, nQ = 0, nJ = 0;
    const tint mnM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, mnM, mArg.msize());

    basic_scmatrix<double, std::complex<double> > mTmp;
    const std::complex<double>* pd = mArg._pd();

    if (pd == m.get())
    {
        mTmp << mArg;
        pd = mTmp;
    }

    ZMEXPC(&mnM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), &tol, &nR, &nI, &nQ, &nJ);
    basic_cvector<double, std::complex<double> > vR(nR);
    basic_array<tint,tint> vI(nI);

    const tint issymm = 0;
    std::complex<double> work_dummy(0.);
    const tint lwork_dummy = 0;

    ZMEXP(&mnM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), m, m._pld(),
           vR, vI, &nR, &nI, &nQ, &nJ, &issymm, &work_dummy, &lwork_dummy);
}

template<>
CVM_API void
__exp_symm<basic_schmatrix<float, std::complex<float> >, float>
    (basic_schmatrix<float, std::complex<float> >& m, 
    const basic_schmatrix<float, std::complex<float> >& mArg, 
    float tol) throw(cvmexception)
{
    tint nR = 0, nI = 0, nQ = 0, nJ = 0;
    const tint nM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, mArg.msize());

    basic_schmatrix<float, std::complex<float> > mTmp;
    const std::complex<float>* pd = mArg._pd();

    if (pd == m.get())
    {
        mTmp << mArg;
        pd = mTmp;
    }

    CMEXPC(&nM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), &tol, &nR, &nI, &nQ, &nJ);
    basic_cvector<float, std::complex<float> > vR(nR);
    basic_array<tint,tint> vI(nI);

    const tint ishem = 1;
    const tint lwork  = 64 * nM;
    basic_cvector<float, std::complex<float> > work(lwork);

    CMEXP(&nM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), m, m._pld(),
           vR, vI, &nR, &nI, &nQ, &nJ, &ishem, work, &lwork);
}

template<>
CVM_API void
__exp_symm<basic_schmatrix<double, std::complex<double> >, double>
    (basic_schmatrix<double, std::complex<double> >& m, 
    const basic_schmatrix<double, std::complex<double> >& mArg, 
    double tol) throw(cvmexception)
{
    tint nR = 0, nI = 0, nQ = 0, nJ = 0;
    const tint nM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, mArg.msize());

    basic_schmatrix<double, std::complex<double> > mTmp;
    const std::complex<double>* pd = mArg._pd();

    if (pd == m.get())
    {
        mTmp << mArg;
        pd = mTmp;
    }

    ZMEXPC(&nM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), &tol, &nR, &nI, &nQ, &nJ);
    basic_cvector<double, std::complex<double> > vR(nR);
    basic_array<tint,tint> vI(nI);

    const tint ishem = 1;
    const tint lwork  = 64 * nM;
    basic_cvector<double, std::complex<double> > work(lwork);

    ZMEXP(&nM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), m, m._pld(),
           vR, vI, &nR, &nI, &nQ, &nJ, &ishem, work, &lwork);
}

template<>
CVM_API void
__cond_num<float, basic_scmatrix<float, std::complex<float> > >
    (const basic_scmatrix<float, std::complex<float> >& mArg, float& dCond) throw(cvmexception)
{
    dCond = 0.F;
    const tint mnM = mArg.msize();
    tint nOutInfo = 0;
    basic_scmatrix<float, std::complex<float> > mA(mArg);
    basic_cvector<float, std::complex<float> > work(mnM * 2);
    basic_rvector<float> rwork(mnM * 2);
    basic_array<tint,tint> iwork(mnM);

    const float rNorm = mA.norminf();
    CGETRF (&mnM, &mnM, mA, mA._pld(), iwork, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    if (nOutInfo == 0)
    {
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
        CGECON(Chars::pI(), 1,
#else
        CGECON(Chars::pI(),
#endif
                &mnM, mA, mA._pld(), &rNorm, &dCond, work, rwork, &nOutInfo);
    }
}

template<>
CVM_API void
__cond_num<double, basic_scmatrix<double, std::complex<double> > >
    (const basic_scmatrix<double, std::complex<double> >& mArg, double& dCond) throw(cvmexception)
{
    dCond = 0.;
    const tint mnM = mArg.msize();
    tint nOutInfo = 0;
    basic_scmatrix<double, std::complex<double> > mA(mArg);
    basic_cvector<double, std::complex<double> > work(mnM * 2);
    basic_rvector<double> rwork(mnM * 2);
    basic_array<tint,tint> iwork(mnM);

    const double rNorm = mA.norminf();
    ZGETRF (&mnM, &mnM, mA, mA._pld(), iwork, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    if (nOutInfo == 0)
    {
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
        ZGECON(Chars::pI(), 1,
#else
        ZGECON(Chars::pI(),
#endif
                &mnM, mA, mA._pld(), &rNorm, &dCond, work, rwork, &nOutInfo);
    }
}

template <>
CVM_API void
__inv<basic_scmatrix<float, std::complex<float> > >
    (basic_scmatrix<float, std::complex<float> >& m,
    const basic_scmatrix<float, std::complex<float> >& mArg) throw(cvmexception)
{
    const tint mnM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, mnM, mArg.msize());

    if (mnM == 1)
    {
        if (_abs(mArg(CVM0,CVM0)) <= basic_cvmMachMin<float>()) {
            throw cvmexception(CVM_SINGULARMATRIX, 1);
        }
        m(CVM0,CVM0) = 1.F / mArg(CVM0,CVM0);
    }
    else
    {
        basic_array<tint,tint> nPivots(mnM);
        m.low_up(mArg, nPivots);

        tint lWork = -1;
        tint nOutInfo = 0;
        std::complex<float> dWork;
        CGETRI(&mnM, m, m._pld(), nPivots, &dWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lWork = static_cast<tint>(dWork.real());
        basic_cvector<float, std::complex<float> > vWork(lWork);

        CGETRI(&mnM, m, m._pld(), nPivots, vWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        _check_positive(CVM_SINGULARMATRIX, nOutInfo);
    }
}

template<>
CVM_API void
__inv<basic_scmatrix<double, std::complex<double> > >
    (basic_scmatrix<double, std::complex<double> >& m,
    const basic_scmatrix<double, std::complex<double> >& mArg) throw(cvmexception)
{
    const tint mnM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, mnM, mArg.msize());

    if (mnM == 1)
    {
        if (_abs(mArg(CVM0,CVM0)) <= basic_cvmMachMin<double>()) {
            throw cvmexception(CVM_SINGULARMATRIX, 1);
        }
        m(CVM0,CVM0) = 1. / mArg(CVM0,CVM0);
    }
    else
    {
        basic_array<tint,tint> nPivots(mnM);
        m.low_up(mArg, nPivots);

        tint lWork = -1;
        tint nOutInfo  = 0;
        std::complex<double> dWork;
        ZGETRI(&mnM, m, m._pld(), nPivots, &dWork, &lWork, &nOutInfo);
        lWork = static_cast<tint>(dWork.real());
        basic_cvector<double, std::complex<double> > vWork(lWork);

        ZGETRI(&mnM, m, m._pld(), nPivots, vWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        _check_positive(CVM_SINGULARMATRIX, nOutInfo);
    }
}

template<>
CVM_API void
__inv<basic_schmatrix<float, std::complex<float> > >
    (basic_schmatrix<float, std::complex<float> >& m,
     const basic_schmatrix<float, std::complex<float> >& mArg) throw(cvmexception)
{
    const tint nM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, mArg.msize());

    if (nM == 1)
    {
        if (_abs(mArg(CVM0,CVM0)) <= basic_cvmMachMin<float>()) {
            throw cvmexception(CVM_SINGULARMATRIX, 1);
        }
        m(CVM0,CVM0) = 1.F / mArg(CVM0,CVM0);
    }
    else
    {
        bool bPositiveDefinite = false;
        tint nOutInfo = 0;
        basic_array<tint,tint> nPivots(nM);

        m._factorize(mArg, nPivots, bPositiveDefinite);

        if (bPositiveDefinite)
        {
            CPOTRI(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                    1,
#endif
                    &nM, m, m._pld(), &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_WRONGCHOLESKYFACTOR, nOutInfo);
        }
        else
        {
            basic_cvector<float, std::complex<float> > vWork(nM);
            CHETRI(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                    1,
#endif
                    &nM, m, m._pld(), nPivots, vWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_WRONGBUNCHKAUFMANFACTOR, nOutInfo);
        }
        m._flip();
    }
}

template<>
CVM_API void
__inv<basic_schmatrix<double, std::complex<double> > >
    (basic_schmatrix<double, std::complex<double> >& m,
     const basic_schmatrix<double, std::complex<double> >& mArg) throw(cvmexception)
{
    const tint nM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, mArg.msize());

    if (nM == 1)
    {
        if (_abs(mArg(CVM0,CVM0)) <= basic_cvmMachMin<double>()) {
            throw cvmexception(CVM_SINGULARMATRIX, 1);
        }
        m(CVM0,CVM0) = 1. / mArg(CVM0,CVM0);
    }
    else
    {
        bool bPositiveDefinite = false;
        tint nOutInfo = 0;
        basic_array<tint,tint> nPivots(nM);

        m._factorize(mArg, nPivots, bPositiveDefinite);

        if (bPositiveDefinite)
        {
            ZPOTRI(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                    1,
#endif
                    &nM, m, m._pld(), &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_WRONGCHOLESKYFACTOR, nOutInfo);
        }
        else
        {
            basic_cvector<double, std::complex<double> > vWork(nM);
            ZHETRI(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                    1,
#endif
                    &nM, m, m._pld(), nPivots, vWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_WRONGBUNCHKAUFMANFACTOR, nOutInfo);
        }
        m._flip();
    }
}

//! @cond SPECIALIZATIONS
template<>
CVM_API void
__polynom<std::complex<float>, basic_cvector<float, std::complex<float> > >
    (std::complex<float>* mpd, tint ldP,
    tint mnM,
    const std::complex<float>* pd, tint ldA,
    const basic_cvector<float, std::complex<float> >& v)
{
    basic_cvector<float, std::complex<float> > vWork(NPOLY(&mnM, v._psize()));
    CPOLY(&mnM, pd, &ldA, v._psize(), v, mpd, &ldP, vWork);
}

template<>
CVM_API void
__polynom<std::complex<double>, basic_cvector<double, std::complex<double> > >
    (std::complex<double>* mpd, tint ldP,
    tint mnM,
    const std::complex<double>* pd, tint ldA,
    const basic_cvector<double, std::complex<double> >& v)
{
    basic_cvector<double, std::complex<double> > vWork(NPOLY(&mnM, v._psize()));
    ZPOLY(&mnM, pd, &ldA, v._psize(), v, mpd, &ldP, vWork);
}

// internal solver
// do not forget to make pX equal to pB before call
// in case of vectors passed increment MUST be 1 (since those methods assume matrices)
template<>
CVM_API void
__solve<float, std::complex<float>, basic_scmatrix<float, std::complex<float> > >
    (const basic_scmatrix<float, std::complex<float> >& m,
    tint nrhs, 
    const std::complex<float>* pB, tint ldB,
    std::complex<float>* pX, tint ldX,
    float& dErr,
    const std::complex<float>* pLU, const tint* pPivots, int transp_mode) throw(cvmexception)
{
    const tint mnM = m.msize();
    const bool bGivenLU = pLU != nullptr && pPivots != nullptr;
    tint nOutInfo = 0;
    basic_rvector<float> vFerr(nrhs);
    basic_rvector<float> vBerr(nrhs);
    basic_cvector<float, std::complex<float> > vWork(2 * mnM);
    basic_rvector<float> rWork(mnM);
    basic_array<tint,tint> nPivots(mnM);
    // 0 - none, 1 - just transposed, 2 - conjugated
    const char* transp = transp_mode == 0 ? Chars::pN() : (transp_mode == 1 ? Chars::pT() : Chars::pC());

    if (bGivenLU) nPivots.assign(pPivots);
    basic_scmatrix<float, std::complex<float> > mLU(mnM);
    if (bGivenLU)
    {
        mLU.assign(pLU);
    }
    else
    {
        mLU = m.low_up(nPivots);
    }
    dErr = 0.F;

    CGETRS(transp,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &nrhs, mLU, mLU._pld(), nPivots, pX, &ldX, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    CGERFS(transp,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &nrhs,
            m, m._pld(),
            mLU, mLU._pld(),
            nPivots,
            pB, &ldB,
            pX, &ldX,
            vFerr, vBerr, vWork, rWork, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    dErr = vFerr.norminf();
}

template<>
CVM_API void
__solve<double, std::complex<double>, basic_scmatrix<double, std::complex<double> > >
    (const basic_scmatrix<double, std::complex<double> >& m,
    tint nrhs,
    const std::complex<double>* pB, tint ldB,
    std::complex<double>* pX, tint ldX,
    double& dErr,
    const std::complex<double>* pLU, const tint* pPivots, int transp_mode) throw(cvmexception)
{
    const tint mnM = m.msize();
    const bool bGivenLU = pLU != nullptr && pPivots != nullptr;
    tint nOutInfo = 0;
    basic_rvector<double> vBerr(nrhs);
    basic_rvector<double> vFerr(nrhs);
    basic_cvector<double, std::complex<double> > vWork(2 * mnM);
    basic_rvector<double> rWork(mnM);
    basic_array<tint,tint> nPivots(mnM);
    // 0 - none, 1 - just transposed, 2 - conjugated
    const char* transp = transp_mode == 0 ? Chars::pN() : (transp_mode == 1 ? Chars::pT() : Chars::pC());

    if (bGivenLU) nPivots.assign(pPivots);
    basic_scmatrix<double, std::complex<double> > mLU(mnM);
    if (bGivenLU)
    {
        mLU.assign(pLU);
    }
    else
    {
        mLU = m.low_up(nPivots);
    }
    dErr = 0.;

    ZGETRS(transp,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &nrhs, mLU, mLU._pld(), nPivots, pX, &ldX, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    ZGERFS(transp,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &nrhs,
            m, m._pld(),
            mLU, mLU._pld(),
            nPivots,
            pB, &ldB,
            pX, &ldX,
            vFerr, vBerr, vWork, rWork, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    dErr = vFerr.norminf();
}

template<>
CVM_API void
__solve<float, std::complex<float>, basic_scbmatrix<float, std::complex<float> > >
    (const basic_scbmatrix<float, std::complex<float> >& m,
    tint nrhs,
    const std::complex<float>* pB, tint ldB,
    std::complex<float>* pX, tint ldX,
    float& dErr,
    const std::complex<float>* pLU, const tint* pPivots, int transp_mode) throw(cvmexception)
{
    const tint mnM = m.msize();
    const tint mnKL = m.lsize();
    const tint mnKU = m.usize();
    const bool bGivenLU = pLU != nullptr && pPivots != nullptr;
    tint nOutInfo = 0;
    basic_rvector<float> vFerr(nrhs);
    basic_rvector<float> vBerr(nrhs);
    basic_cvector<float, std::complex<float> > vWork(2 * mnM);
    basic_rvector<float> rWork(mnM);
    basic_array<tint,tint>nPivots(mnM);
    // 0 - none, 1 - just transposed, 2 - conjugated
    const char* transp = transp_mode == 0 ? Chars::pN() : (transp_mode == 1 ? Chars::pT() : Chars::pC());

    if (bGivenLU) nPivots.assign(pPivots);
    basic_scbmatrix<float, std::complex<float> > mLU(mnM, mnKL, mnKL + mnKU);
    if (bGivenLU)
    {
        mLU.assign(pLU);
    }
    else
    {
        mLU = m.low_up(nPivots);
    }
    dErr = 0.F;

    CGBTRS(transp,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &mnKL, &mnKU, &nrhs, mLU, mLU._pld(), nPivots, pX, &ldX, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    CGBRFS(transp,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &mnKL, &mnKU, &nrhs,
            m, m._pld(),
            mLU, mLU._pld(),
            nPivots,
            pB, &ldB,
            pX, &ldX,
            vFerr, vBerr, vWork, rWork, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    dErr = vFerr.norminf();
}

template<>
CVM_API void
__solve<double, std::complex<double>, basic_scbmatrix<double, std::complex<double> > >
    (const basic_scbmatrix<double, std::complex<double> >& m,
    tint nrhs,
    const std::complex<double>* pB, tint ldB,
    std::complex<double>* pX, tint ldX,
    double& dErr,
    const std::complex<double>* pLU, const tint* pPivots, int transp_mode) throw(cvmexception)
{
    const tint mnM = m.msize();
    const tint mnKL = m.lsize();
    const tint mnKU = m.usize();
    const bool bGivenLU = pLU != nullptr && pPivots != nullptr;
    tint nOutInfo = 0;
    basic_rvector<double> vFerr(nrhs);
    basic_rvector<double> vBerr(nrhs);
    basic_cvector<double, std::complex<double> > vWork(2 * mnM);
    basic_rvector<double> rWork(mnM);
    basic_array<tint,tint> nPivots(mnM);
    // 0 - none, 1 - just transposed, 2 - conjugated
    const char* transp = transp_mode == 0 ? Chars::pN() : (transp_mode == 1 ? Chars::pT() : Chars::pC());

    if (bGivenLU) nPivots.assign(pPivots);
    basic_scbmatrix<double, std::complex<double> > mLU(mnM, mnKL, mnKL + mnKU);
    if (bGivenLU)
    {
        mLU.assign(pLU);
    }
    else
    {
        mLU = m.low_up(nPivots);
    }
    dErr = 0.;

    ZGBTRS(transp,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &mnKL, &mnKU, &nrhs, mLU, mLU._pld(), nPivots, pX, &ldX, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    ZGBRFS(transp,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &mnKL, &mnKU, &nrhs,
            m, m._pld(),
            mLU, mLU._pld(),
            nPivots,
            pB, &ldB,
            pX, &ldB,
            vFerr, vBerr, vWork, rWork, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    dErr = vFerr.norminf();
}


// internal solver
// don't forget to make pX equal to pB before call
template<>
CVM_API void
__solve<float, std::complex<float>, basic_schmatrix<float, std::complex<float> > >
    (const basic_schmatrix<float, std::complex<float> >& m,
    tint nrhs,
    const std::complex<float>* pB, tint ldB,
    std::complex<float>* pX, tint ldX,
    float& dErr,
    const std::complex<float>* pLU, const tint* pPivots, int) throw(cvmexception)
{
    const tint nM = m.msize();
    const bool bCholeskyGiven = pLU != nullptr && pPivots == nullptr;        // no pivots means Cholesky
    const bool bBunchKaufmanGiven = pLU != nullptr && pPivots != nullptr;
    const bool bCalc = !bCholeskyGiven && !bBunchKaufmanGiven;
    bool bPositiveDefinite = bCholeskyGiven;

    tint nOutInfo = 0;
    basic_rvector<float> vBerr(nrhs);
    basic_rvector<float> vFerr(nrhs);
    basic_cvector<float, std::complex<float> > vWork(2 * nM);
    basic_rvector<float> vrWork(nM);
    basic_array<tint,tint> nPivots(nM);

    if (bBunchKaufmanGiven) nPivots.assign(pPivots);
    basic_schmatrix<float, std::complex<float> > mLU(nM);
    if (bCalc)
    {
        mLU._factorize(m, nPivots, bPositiveDefinite);
    }
    else
    {
        mLU.assign(pLU);
    }
    dErr = 0.F;

    if (bPositiveDefinite)
    {
        CPOTRS(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs, mLU, mLU._pld(), pX, &ldX, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        CPORFS(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs, 
                m, m._pld(),
                mLU, mLU._pld(),
                pB, &ldB,
                pX, &ldX,
                vFerr, vBerr, 
                vWork, vrWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }
    else
    {
        CHETRS(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs, mLU, mLU._pld(), nPivots, pX, &ldX, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        CHERFS(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs,
                m, m._pld(),
                mLU, mLU._pld(),
                nPivots,
                pB, &ldB,
                pX, &ldX,
                vFerr, vBerr, 
                vWork, vrWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }

    dErr = vFerr.norminf();
}

template<>
CVM_API void
__solve<double, std::complex<double>, basic_schmatrix<double, std::complex<double> > >
    (const basic_schmatrix<double, std::complex<double> >& m,
    tint nrhs,
    const std::complex<double>* pB, tint ldB,
    std::complex<double>* pX, tint ldX,
    double& dErr,
    const std::complex<double>* pLU, const tint* pPivots, int) throw(cvmexception)
{
    const tint nM = m.msize();
    const bool bCholeskyGiven = pLU != nullptr && pPivots == nullptr;        // no pivots means Cholesky
    const bool bBunchKaufmanGiven = pLU != nullptr && pPivots != nullptr;
    const bool bCalc = !bCholeskyGiven && !bBunchKaufmanGiven;
    bool bPositiveDefinite = bCholeskyGiven;

    tint nOutInfo = 0;
    basic_rvector<double> vBerr(nrhs);
    basic_rvector<double> vFerr(nrhs);
    basic_cvector<double, std::complex<double> > vWork(2 * nM);
    basic_rvector<double> vrWork(nM);
    basic_array<tint,tint> nPivots(nM);

    if (bBunchKaufmanGiven) nPivots.assign(pPivots);
    basic_schmatrix<double, std::complex<double> > mLU(nM);
    if (bCalc)
    {
        mLU._factorize(m, nPivots, bPositiveDefinite);
    }
    else
    {
        mLU.assign(pLU);
    }
    dErr = 0.;

    if (bPositiveDefinite)
    {
        ZPOTRS(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs, mLU, mLU._pld(), pX, &ldX, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        ZPORFS(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs, 
                m, m._pld(),
                mLU, mLU._pld(),
                pB, &ldB,
                pX, &ldX,
                vFerr, vBerr, 
                vWork, vrWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }
    else
    {
        ZHETRS(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs, mLU, mLU._pld(), nPivots, pX, &ldX, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        ZHERFS(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs,
                m, m._pld(),
                mLU, mLU._pld(),
                nPivots,
                pB, &ldB,
                pX, &ldX,
                vFerr, vBerr, 
                vWork, vrWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }

    dErr = vFerr.norminf();
}
//! @endcond

CVM_NAMESPACE_END

