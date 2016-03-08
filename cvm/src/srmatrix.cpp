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
__exp<basic_srmatrix<float>, float>
    (basic_srmatrix<float>& m,
    const basic_srmatrix<float>& mArg,
    float tol) throw (cvmexception)
{
    tint nR = 0, nI = 0, nQ = 0, nJ = 0;
    const tint nM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, mArg.msize());

    basic_srmatrix<float> mTmp;
    const float* pd = mArg._pd();

    if (pd == m.get())
    {
        mTmp << mArg;
        pd = mTmp;
    }

    SMEXPC (&nM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), &tol, &nR, &nI, &nQ, &nJ);
    basic_rvector<float> vR (nR);
    basic_array<tint,tint> vI (nI);

    const tint issymm = 0;
    float work_dummy = 0.F;
    const tint lwork_dummy = 0;

    SMEXP (&nM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), m, m._pld(), 
           vR, vI, &nR, &nI, &nQ, &nJ, &issymm, &work_dummy, &lwork_dummy);
}

template<>
CVM_API void 
__exp<basic_srmatrix<double>, double>
    (basic_srmatrix<double>& m,
    const basic_srmatrix<double>& mArg,
    double tol) throw (cvmexception)
{
    tint nR = 0, nI = 0, nQ = 0, nJ = 0;
    const tint nM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, mArg.msize());

    basic_srmatrix<double> mTmp;
    const double* pd = mArg._pd();

    if (pd == m.get())
    {
        mTmp << mArg;
        pd = mTmp;
    }

    DMEXPC (&nM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), &tol, &nR, &nI, &nQ, &nJ);
    basic_rvector<double> vR (nR);
    basic_array<tint,tint> vI (nI);

    const tint issymm = 0;
    double work_dummy = 0.;
    const tint lwork_dummy = 0;

    DMEXP (&nM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), m, m._pld(),
           vR, vI, &nR, &nI, &nQ, &nJ, &issymm, &work_dummy, &lwork_dummy);
}

template<>
CVM_API void
__exp_symm<basic_srsmatrix<float>, float>
    (basic_srsmatrix<float>& m, 
    const basic_srsmatrix<float>& mArg, 
    float tol) throw (cvmexception)
{
    tint nR = 0, nI = 0, nQ = 0, nJ = 0;
    const tint nM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, mArg.msize());

    basic_srmatrix<float> mTmp;
    const float* pd = mArg._pd();

    if (pd == m.get())
    {
        mTmp << mArg;
        pd = mTmp;
    }

    SMEXPC (&nM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), &tol, &nR, &nI, &nQ, &nJ);
    basic_rvector<float> vR (nR);
    basic_array<tint,tint> vI (nI);

    const tint issymm = 1;
    const tint lwork  = 64 * nM;
    basic_rvector<float> work (lwork);

    SMEXP (&nM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), m, m._pld(),
           vR, vI, &nR, &nI, &nQ, &nJ, &issymm, work, &lwork);
}

template<>
CVM_API void
__exp_symm<basic_srsmatrix<double>, double>
    (basic_srsmatrix<double>& m,
    const basic_srsmatrix<double>& mArg,
    double tol) throw (cvmexception)
{
    tint nR = 0, nI = 0, nQ = 0, nJ = 0;
    const tint nM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, mArg.msize());

    basic_srmatrix<double> mTmp;
    const double* pd = mArg._pd();

    if (pd == m.get())
    {
        mTmp << mArg;
        pd = mTmp;
    }

    DMEXPC (&nM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), &tol, &nR, &nI, &nQ, &nJ);
    basic_rvector<double> vR (nR);
    basic_array<tint,tint> vI (nI);

    const tint issymm = 1;
    const tint lwork  = 64 * nM;
    basic_rvector<double> work (lwork);

    DMEXP (&nM, pd, pd == m.get() ? mTmp._pld() : mArg._pldm(), m, m._pld(),
           vR, vI, &nR, &nI, &nQ, &nJ, &issymm, work, &lwork);
}

template<>
CVM_API void
__cond_num<float, basic_srmatrix<float> >
    (const basic_srmatrix<float>& mArg, float& dCond) throw (cvmexception)
{
    dCond = 0.F;
    const tint mnM = mArg.msize();
    tint nOutInfo = 0;
    basic_srmatrix<float> mA (mArg);
    basic_rvector<float> work (mnM * 4);
    basic_array<tint,tint> iwork (mnM);

    const float rNorm = mA.norminf();
    SGETRF (&mnM, &mnM, mA, mA._pld(), iwork, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    if (nOutInfo == 0)
    {
        SGECON (Chars::pI(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &mnM, mA, mA._pld(), &rNorm, &dCond, work, iwork, &nOutInfo);
    }
}

template<>
CVM_API void
__cond_num<double, basic_srmatrix<double> >
    (const basic_srmatrix<double>& mArg, double& dCond) throw (cvmexception)
{
    dCond = 0.;
    const tint mnM = mArg.msize();
    tint nOutInfo = 0;
    basic_srmatrix<double> mA (mArg);
    basic_rvector<double> work (mnM * 4);
    basic_array<tint,tint> iwork (mnM);

    const double rNorm = mA.norminf();
    DGETRF (&mnM, &mnM, mA, mA._pld(), iwork, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    if (nOutInfo == 0)
    {
        DGECON (Chars::pI(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &mnM, mA, mA._pld(), &rNorm, &dCond, work, iwork, &nOutInfo);
    }
}

template<>
CVM_API void
__inv<basic_srmatrix<float> >
    (basic_srmatrix<float>& m,
    const basic_srmatrix<float>& mArg) throw (cvmexception)
{
    const tint mnM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, mnM, mArg.msize());
    if (mnM == 1)
    {
        if (_abs (mArg(CVM0,CVM0)) <= basic_cvmMachMin<float>()) {
            throw cvmexception (CVM_SINGULARMATRIX, 1);
        }
        m(CVM0,CVM0) = 1.F / mArg(CVM0,CVM0);
    }
    else
    {
        basic_array<tint,tint> nPivots (mnM);
        m.low_up (mArg, nPivots);

        tint lWork = -1;
        tint nOutInfo = 0;
        float dWork;
        SGETRI (&mnM, m, m._pld(), nPivots, &dWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lWork = static_cast<tint>(dWork);
        basic_rvector<float> vWork (lWork);

        SGETRI (&mnM, m, m._pld(), nPivots, vWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        _check_positive(CVM_SINGULARMATRIX, nOutInfo);
    }
}

template<>
CVM_API void
__inv<basic_srmatrix<double> >
    (basic_srmatrix<double>& m,
    const basic_srmatrix<double>& mArg) throw (cvmexception)
{
    const tint mnM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, mnM, mArg.msize());

    if (mnM == 1)
    {
        if (_abs (mArg(CVM0,CVM0)) <= basic_cvmMachMin<double>()) {
            throw cvmexception (CVM_SINGULARMATRIX, 1);
        }
        m(CVM0,CVM0) = 1. / mArg(CVM0,CVM0);
    }
    else
    {
        basic_array<tint,tint> nPivots (mnM);
        m.low_up (mArg, nPivots);

        tint lWork = -1;
        tint nOutInfo  = 0;
        double dWork;
        DGETRI (&mnM, m, m._pld(), nPivots, &dWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lWork = static_cast<tint>(dWork);
        basic_rvector<double> vWork (lWork);

        DGETRI (&mnM, m, m._pld(), nPivots, vWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        _check_positive(CVM_SINGULARMATRIX, nOutInfo);
    }
}

template<>
CVM_API void
__polynom<float, basic_rvector<float> >
    (float* mpd, tint ldP, tint mnM, const float* pd, tint ldA, const basic_rvector<float>& v)
{
    basic_rvector<float> vWork (NPOLY (&mnM, v._psize()));
    SPOLY (&mnM, pd, &ldA, v._psize(), v, mpd, &ldP, vWork);
}

template<>
CVM_API void
__polynom<double, basic_rvector<double> >
    (double* mpd, tint ldP, tint mnM, const double* pd, tint ldA, const basic_rvector<double>& v)
{
    basic_rvector<double> vWork (NPOLY (&mnM, v._psize()));
    DPOLY (&mnM, pd, &ldA, v._psize(), v, mpd, &ldP, vWork);
}

// internal solver
// don't forget to make pX equal to pB before call
template<>
CVM_API void
__solve<float, float, basic_srmatrix<float> >
    (const basic_srmatrix<float>& m,
    tint nrhs,
    const float* pB, tint ldB, 
    float* pX, tint ldX,
    float& dErr,
    const float* pLU, const tint* pPivots, int transp_mode) throw (cvmexception)
{
    const tint mnM = m.msize();
    const bool bGivenLU = pLU != nullptr && pPivots != nullptr;
    tint nOutInfo = 0;
    basic_rvector<float> vBerr (nrhs);
    basic_rvector<float> vFerr (nrhs);
    basic_rvector<float> vWork (3 * mnM);
    basic_array<tint,tint> iWork (mnM);
    basic_array<tint,tint> nPivots (mnM);
    const char* transp = transp_mode == 0 ? Chars::pN() : Chars::pT();

    if (bGivenLU) nPivots.assign (pPivots);
    basic_srmatrix<float> mLU (mnM);
    if (bGivenLU)
    {
        mLU.assign (pLU);
    }
    else
    {
        mLU = m.low_up (nPivots);
    }
    dErr = 0.F;

    SGETRS (transp,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &nrhs, mLU, mLU._pld(), nPivots, pX, &ldX, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    SGERFS (transp,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &nrhs,
            m, m._pld(),
            mLU, mLU._pld(),
            nPivots,
            pB, &ldB,
            pX, &ldX,
            vFerr, vBerr, vWork, iWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    dErr = vFerr.norminf();
}

template<>
CVM_API void
__solve<double, double, basic_srmatrix<double> >
    (const basic_srmatrix<double>& m,
    tint nrhs,
    const double* pB, tint ldB,
    double* pX, tint ldX,
    double& dErr,
    const double* pLU, const tint* pPivots, int transp_mode) throw (cvmexception)
{
    const tint mnM = m.msize();
    const bool bGivenLU = pLU != nullptr && pPivots != nullptr;
    tint nOutInfo = 0;
    basic_rvector<double> vBerr (nrhs);
    basic_rvector<double> vFerr (nrhs);
    basic_rvector<double> vWork (3 * mnM);
    basic_array<tint,tint> iWork (mnM);
    basic_array<tint,tint> nPivots (mnM);
    const char* transp = transp_mode == 0 ? Chars::pN() : Chars::pT();

    if (bGivenLU) nPivots.assign (pPivots);
    basic_srmatrix<double> mLU (mnM);
    if (bGivenLU)
    {
        mLU.assign (pLU);
    }
    else
    {
        mLU = m.low_up(nPivots);
    }
    dErr = 0.;

    DGETRS (transp,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &nrhs, mLU, mLU._pld(), nPivots, pX, &ldX, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    DGERFS (transp,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &nrhs,
            m, m._pld(),
            mLU, mLU._pld(),
            nPivots,
            pB, &ldB,
            pX, &ldX,
            vFerr, vBerr, vWork, iWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    dErr = vFerr.norminf();
}

template<>
CVM_API void
__solve<float, float, basic_srbmatrix<float> >
    (const basic_srbmatrix<float>& m,
    tint nrhs,
    const float* pB, tint ldB,
    float* pX, tint ldX,
    float& dErr, 
    const float* pLU, const tint* pPivots, int transp_mode) throw (cvmexception)
{
    const tint mnM = m.msize();
    const tint mnKL = m.lsize();
    const tint mnKU = m.usize();
    const bool bGivenLU = pLU != nullptr && pPivots != nullptr;
    tint nOutInfo = 0;
    basic_rvector<float> vBerr (nrhs);
    basic_rvector<float> vFerr (nrhs);
    basic_rvector<float> vWork (3 * mnM);
    basic_array<tint,tint> iWork (mnM);
    basic_array<tint,tint> nPivots (mnM);
    const char* transp = transp_mode == 0 ? Chars::pN() : Chars::pT();

    if (bGivenLU) nPivots.assign (pPivots);
    basic_srbmatrix<float> mLU (mnM, mnKL, mnKL + mnKU);
    if (bGivenLU)
    {
        mLU.assign (pLU);
    }
    else
    {
        mLU = m.low_up(nPivots);
    }
    dErr = 0.F;

    SGBTRS (transp,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &mnKL, &mnKU, &nrhs, mLU, mLU._pld(), nPivots, pX, &ldX, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    SGBRFS (transp,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &mnKL, &mnKU, &nrhs,
            m, m._pld(),
            mLU, mLU._pld(),
            nPivots,
            pB, &ldB,
            pX, &ldX,
            vFerr, vBerr, vWork, iWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    dErr = vFerr.norminf();
}

template<>
CVM_API void
__solve<double, double, basic_srbmatrix<double> >
    (const basic_srbmatrix<double>& m,
    tint nrhs,
    const double* pB, tint ldB,
    double* pX, tint ldX,
    double& dErr,
    const double* pLU, const tint* pPivots, int transp_mode) throw (cvmexception)
{
    const tint mnM = m.msize();
    const tint mnKL = m.lsize();
    const tint mnKU = m.usize();
    const bool bGivenLU = pLU != nullptr && pPivots != nullptr;
    tint nOutInfo = 0;
    basic_rvector<double> vBerr (nrhs);
    basic_rvector<double> vFerr (nrhs);
    basic_rvector<double> vWork (3 * mnM);
    basic_array<tint,tint> iWork (mnM);
    basic_array<tint,tint> nPivots (mnM);
    const char* transp = transp_mode == 0 ? Chars::pN() : Chars::pT();

    if (bGivenLU) nPivots.assign (pPivots);
    basic_srbmatrix<double> mLU (mnM, mnKL, mnKL + mnKU);
    if (bGivenLU)
    {
        mLU.assign (pLU);
    }
    else
    {
        mLU = m.low_up(nPivots);
    }
    dErr = 0.;

    DGBTRS (transp,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &mnKL, &mnKU, &nrhs, mLU, mLU._pld(), nPivots, pX, &ldX, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    DGBRFS (transp,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            1,
#endif
            &mnM, &mnKL, &mnKU, &nrhs,
            m, m._pld(),
            mLU, mLU._pld(),
            nPivots,
            pB, &ldB,
            pX, &ldX,
            vFerr, vBerr, vWork, iWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    dErr = vFerr.norminf();
}

// internal solver
// don't forget to make pX equal to pB before call
template<>
CVM_API void
__solve<float, float, basic_srsmatrix<float> >
    (const basic_srsmatrix<float>& m,
    tint nrhs,
    const float* pB, tint ldB,
    float* pX, tint ldX,
    float& dErr,
    const float* pLU, const tint* pPivots, int) throw (cvmexception)
{
    const tint nM = m.msize();
    const bool bCholeskyGiven = pLU != nullptr && pPivots == nullptr;        // no pivots means Cholesky
    const bool bBunchKaufmanGiven = pLU != nullptr && pPivots != nullptr;
    const bool bCalc = !bCholeskyGiven && !bBunchKaufmanGiven;
    bool bPositiveDefinite = bCholeskyGiven;

    tint nOutInfo = 0;
    basic_rvector<float> vBerr (nrhs);
    basic_rvector<float> vFerr (nrhs);
    basic_rvector<float> vWork (3 * nM);
    basic_array<tint,tint> iWork (nM);
    basic_array<tint,tint> nPivots (nM);

    if (bBunchKaufmanGiven) nPivots.assign (pPivots);
    basic_srsmatrix<float> mLU(nM);
    if (bCalc)
    {
        mLU._factorize (m, nPivots, bPositiveDefinite);
    }
    else
    {
        mLU.assign (pLU);
    }
    dErr = 0.F;

    if (bPositiveDefinite)
    {
        SPOTRS (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs, mLU, mLU._pld(), pX, &ldX, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        SPORFS (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs, 
                m, m._pld(),
                mLU, mLU._pld(),
                pB, &ldB,
                pX, &ldX,
                vFerr, vBerr, 
                vWork, iWork, &nOutInfo);

        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }
    else
    {
        SSYTRS (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs, mLU, mLU._pld(), nPivots, pX, &ldX, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        SSYRFS (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs,
                m, m._pld(),
                mLU, mLU._pld(),
                nPivots,
                pB, &ldB,
                pX, &ldX,
                vFerr, vBerr, 
                vWork, iWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }

    dErr = vFerr.norminf();
}

template<>
CVM_API void
__solve<double, double, basic_srsmatrix<double> >
    (const basic_srsmatrix<double>& m,
    tint nrhs,
    const double* pB, tint ldB,
    double* pX, tint ldX,
    double& dErr,
    const double* pLU, const tint* pPivots, int) throw (cvmexception)
{
    const tint nM = m.msize();
    const bool bCholeskyGiven = pLU != nullptr && pPivots == nullptr;        // no pivots means Cholesky
    const bool bBunchKaufmanGiven = pLU != nullptr && pPivots != nullptr;
    const bool bCalc = !bCholeskyGiven && !bBunchKaufmanGiven;
    bool bPositiveDefinite = bCholeskyGiven;

    tint nOutInfo = 0;
    basic_rvector<double> vBerr (nrhs);
    basic_rvector<double> vFerr (nrhs);
    basic_rvector<double> vWork (3 * nM);
    basic_array<tint,tint> iWork (nM);
    basic_array<tint,tint> nPivots (nM);

    if (bBunchKaufmanGiven) nPivots.assign (pPivots);
    basic_srsmatrix<double> mLU(nM);
    if (bCalc)
    {
        mLU._factorize (m, nPivots, bPositiveDefinite);
    }
    else
    {
        mLU.assign (pLU);
    }
    dErr = 0.;

    if (bPositiveDefinite)
    {
        DPOTRS (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs, mLU, mLU._pld(), pX, &ldX, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        DPORFS (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs,
                m, m._pld(),
                mLU, mLU._pld(),
                pB, &ldB,
                pX, &ldX,
                vFerr, vBerr, 
                vWork, iWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }
    else
    {
        DSYTRS (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs, mLU, mLU._pld(), nPivots, pX, &ldX, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        DSYRFS (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                1,
#endif
                &nM, &nrhs,
                m, m._pld(),
                mLU, mLU._pld(),
                nPivots,
                pB, &ldB,
                pX, &ldX,
                vFerr, vBerr, 
                vWork, iWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
    }

    dErr = vFerr.norminf();
}

template<>
CVM_API void
__inv<basic_srsmatrix<float> >
    (basic_srsmatrix<float>& m,
    const basic_srsmatrix<float>& mArg) throw (cvmexception)
{
    const tint nM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, mArg.msize());

    if (nM == 1)
    {
        if (_abs (mArg(CVM0,CVM0)) <= basic_cvmMachMin<float>()) {
            throw cvmexception (CVM_SINGULARMATRIX, 1);
        }
        m.set (CVM0, CVM0, 1.F / mArg(CVM0,CVM0));
    }
    else
    {
        bool bPositiveDefinite = false;
        tint nOutInfo = 0;
        basic_array<tint,tint> nPivots (nM);

        m._factorize (mArg, nPivots, bPositiveDefinite);

        if (bPositiveDefinite)
        {
            SPOTRI (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                    1,
#endif
                    &nM, m, m._pld(), &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_WRONGCHOLESKYFACTOR, nOutInfo);
        }
        else
        {
            basic_rvector<float> vWork (nM);
            SSYTRI (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
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
__inv<basic_srsmatrix<double> >
    (basic_srsmatrix<double>& m,
    const basic_srsmatrix<double>& mArg) throw (cvmexception)
{
    const tint nM = m.msize();
    _check_ne(CVM_SIZESMISMATCH, nM, mArg.msize());

    if (nM == 1)
    {
        if (_abs (mArg(CVM0,CVM0)) <= basic_cvmMachMin<double>()) {
            throw cvmexception (CVM_SINGULARMATRIX, 1);
        }
        m.set (CVM0, CVM0, 1. / mArg(CVM0,CVM0));
    }
    else
    {
        bool bPositiveDefinite = false;
        tint nOutInfo = 0;
        basic_array<tint,tint> nPivots (nM);

        m._factorize (mArg, nPivots, bPositiveDefinite);

        if (bPositiveDefinite)
        {
            DPOTRI (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                    1,
#endif
                    &nM, m, m._pld(), &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_WRONGCHOLESKYFACTOR, nOutInfo);
        }
        else
        {
            basic_rvector<double> vWork (nM);
            DSYTRI (Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                    1,
#endif
                    &nM, m, m._pld(), nPivots, vWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_WRONGBUNCHKAUFMANFACTOR, nOutInfo);
        }
        m._flip();
    }
}

CVM_NAMESPACE_END
