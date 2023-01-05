//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2023
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "cvm.h"
#include "blas.h"

CVM_NAMESPACE_BEG

template<>
CVM_API std::complex<float>
__dotu<std::complex<float>>(const std::complex<float>* mpd, tint mn_size, tint mn_incr,
                            const std::complex<float>* pd, tint incr)
{
#if defined(CVM_COMPLEX_NUMBER_RETURNED)
    return CDOTU(&mn_size, mpd, &mn_incr, pd, &incr);
#else
    std::complex<float> cRes;
    VCDOTU(&cRes, &mn_size, mpd, &mn_incr, pd, &incr);
    return cRes;
#endif
}

template<>
CVM_API std::complex<double>
__dotu<std::complex<double>>(const std::complex<double>* mpd, tint mn_size, tint mn_incr,
                             const std::complex<double>* pd, tint incr)
{
#if defined(CVM_COMPLEX_NUMBER_RETURNED)
    return ZDOTU(&mn_size, mpd, &mn_incr, pd, &incr);
#else
    std::complex<double> cRes;
    VZDOTU(&cRes, &mn_size, mpd, &mn_incr, pd, &incr);
    return cRes;
#endif
}

template<>
CVM_API std::complex<float>
__dotc<std::complex<float>>  (const std::complex<float>* mpd, tint mn_size, tint mn_incr,
                              const std::complex<float>* pd, tint incr)
{
#if defined(CVM_COMPLEX_NUMBER_RETURNED)
    return CDOTC(&mn_size, mpd, &mn_incr, pd, &incr);
#else
    std::complex<float> cRes;
    VCDOTC(&cRes, &mn_size, mpd, &mn_incr, pd, &incr);
    return cRes;
#endif
}

template<>
CVM_API std::complex<double>
__dotc<std::complex<double>>  (const std::complex<double>* mpd, tint mn_size, tint mn_incr,
                               const std::complex<double>* pd, tint incr)
{
#if defined(CVM_COMPLEX_NUMBER_RETURNED)
    return ZDOTC(&mn_size, mpd, &mn_incr, pd, &incr);
#else
    std::complex<double> cRes;
    VZDOTC(&cRes, &mn_size, mpd, &mn_incr, pd, &incr);
    return cRes;
#endif
}

//! @cond SPECIALIZATIONS
template<>
CVM_API void
__gemv<std::complex<float>, basic_cmatrix<float, std::complex<float>> , basic_cvector<float, std::complex<float>>  >
    (bool bLeft,
    const basic_cmatrix<float, std::complex<float>> & m,
    std::complex<float> dAlpha,
    const basic_cvector<float, std::complex<float>> & v,
    std::complex<float> dBeta,
    basic_cvector<float, std::complex<float>> & vRes)
{
    CGEMV(bLeft ? Chars::pT() : Chars::pN(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          m._pm(), m._pn(), &dAlpha, m._pd(), m._pldm(), v, v._pincr(), &dBeta, vRes, vRes._pincr());
}

template<>
CVM_API void
__gemv<std::complex<double>, basic_cmatrix<double, std::complex<double>> , basic_cvector<double, std::complex<double>>  >
    (bool bLeft,
    const basic_cmatrix<double, std::complex<double>> & m,
    std::complex<double> dAlpha,
    const basic_cvector<double, std::complex<double>> & v,
    std::complex<double> dBeta,
    basic_cvector<double, std::complex<double>> & vRes)
{
    ZGEMV(bLeft ? Chars::pT() : Chars::pN(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          m._pm(), m._pn(), &dAlpha, m._pd(), m._pldm(), v, v._pincr(), &dBeta, vRes, vRes._pincr());
}

template<>
CVM_API void
__gbmv<std::complex<float>, basic_scbmatrix<float, std::complex<float>> , basic_cvector<float, std::complex<float>>  >
    (bool bLeft,
    const basic_scbmatrix<float, std::complex<float>> & m,
    std::complex<float> dAlpha,
    const basic_cvector<float, std::complex<float>> & v,
    std::complex<float> dBeta,
    basic_cvector<float, std::complex<float>> & vRes)
{
    CGBMV(bLeft ? Chars::pT() : Chars::pN(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          m._pm(), m._pn(), m._pl(), m._pu(), &dAlpha, m, m._pld(), v, v._pincr(), &dBeta, vRes, vRes._pincr());
}

template<>
CVM_API void
__gbmv<std::complex<double>, basic_scbmatrix<double, std::complex<double>> , basic_cvector<double, std::complex<double>>  >
    (bool bLeft,
    const basic_scbmatrix<double, std::complex<double>> & m,
    std::complex<double> dAlpha,
    const basic_cvector<double, std::complex<double>> & v,
    std::complex<double> dBeta,
    basic_cvector<double, std::complex<double>> & vRes)
{
    ZGBMV(bLeft ? Chars::pT() : Chars::pN(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          m._pm(), m._pn(), m._pl(), m._pu(), &dAlpha, m, m._pld(), v, v._pincr(), &dBeta, vRes, vRes._pincr());
}

template<>
CVM_API void
__shmv<std::complex<float>, basic_schmatrix<float, std::complex<float>> , basic_cvector<float, std::complex<float>>  >
    (const basic_schmatrix<float, std::complex<float>> & m,
     std::complex<float> cAlpha,
     const basic_cvector<float, std::complex<float>> & v,
     std::complex<float> cBeta,
     basic_cvector<float, std::complex<float>> & vRes)
{
    // calling with 'L' for left-sided multiplication does not work here
    CHEMV(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          m._pm(), &cAlpha, m, m._pld(), v, v._pincr(), &cBeta, vRes, vRes._pincr());
}

template<>
CVM_API void
__shmv<std::complex<double>, basic_schmatrix<double, std::complex<double>> , basic_cvector<double, std::complex<double>>  >
    (const basic_schmatrix<double, std::complex<double>> & m,
     std::complex<double> cAlpha,
     const basic_cvector<double, std::complex<double>> & v,
     std::complex<double> cBeta,
     basic_cvector<double, std::complex<double>> & vRes)
{
    // calling with 'L' for left-sided multiplication does not work here
    ZHEMV(Chars::pU(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          m._pm(), &cAlpha, m, m._pld(), v, v._pincr(), &cBeta, vRes, vRes._pincr());
}

template<>
CVM_API void
__eig<basic_cvector<float, std::complex<float>> , basic_srmatrix<float>, basic_scmatrix<float, std::complex<float>>  >
    (basic_cvector<float, std::complex<float>> & vRes,
    const basic_srmatrix<float>& mArg,
    basic_scmatrix<float, std::complex<float>> * mEigVect,
    bool bRightVect)
{
    const bool bEigVect = (mEigVect != nullptr);
    const tint nM = mArg.msize();
    tint lWork = -1;
    tint ilo = 0;
    tint ihi = 0;
    tint nOutInfo = 0;

    _check_ne(CVM_SIZESMISMATCH, vRes.size(), nM);
    if (nM == 1) {
        const std::complex<float> one(1.F, 0.F);
        vRes[0] = std::complex<float>(mArg(0,0), 0.F);
        if (bEigVect) {
            mEigVect -> resize(1);
            (*mEigVect)[0].set(one);
        }
    } else {
        basic_srmatrix<float> mA(mArg);
        basic_rvector<float> vScale(nM);
        basic_rvector<float> vTau(_cvm_max<tint>(1, nM - 1));
        basic_rvector<float> vR(nM);
        basic_rvector<float> vI(nM);

        SGEBAL(Chars::pB(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
               1,
#endif
               &nM, mA, &nM, &ilo, &ihi, vScale, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        float dWork;
        SGEHRD(&nM, &ilo, &ihi, mA, &nM, vTau, &dWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lWork = static_cast<tint>(dWork);
        basic_rvector<float> vWork(lWork);

        SGEHRD(&nM, &ilo, &ihi, mA, &nM, vTau, vWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        if (bEigVect) {
            const tint one(1);
            tint m = 0;
            tint lSelect = 0;
            const tint ldvl = bRightVect ? 1 : nM;
            const tint ldvr = bRightVect ? nM : 1;
            basic_srmatrix<float> vl(ldvl);
            basic_srmatrix<float> vr(ldvr);
            basic_rvector <float> work(3 * nM);
            basic_srmatrix<float> mH(mA);
            const char* pRL = bRightVect ? Chars::pR() : Chars::pL();

            if (bRightVect) {
                vr = mA;
            } else {
                vl = mA;
            }

            lWork = -1;
            SORGHR(&nM, &one, &nM, bRightVect ? vr : vl, &nM, vTau, &dWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            lWork = static_cast<tint>(dWork);
            if (lWork > vWork.size()) vWork.resize(lWork);

            SORGHR(&nM, &one, &nM, bRightVect ? vr : vl, &nM, vTau, vWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);

            lWork = -1;
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            SHSEQR(Chars::pS(), 1, Chars::pV(), 1,
#else
            SHSEQR(Chars::pS(),    Chars::pV(),
#endif
                   &nM, &ilo, &ihi, mH, &nM, vR, vI, bRightVect ? vr : vl, &nM, &dWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            lWork = static_cast<tint>(dWork) * 11;
            if (lWork > vWork.size()) vWork.resize(lWork);

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            SHSEQR(Chars::pS(), 1, Chars::pV(), 1,
#else
            SHSEQR(Chars::pS(),    Chars::pV(),
#endif
                   &nM, &ilo, &ihi, mH, &nM, vR, vI, bRightVect ? vr : vl, &nM, vWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "SHSEQR", __FILE__, __LINE__);

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            STREVC(pRL, 1, Chars::pB(), 1,
#else
            STREVC(pRL,    Chars::pB(),
#endif
                   &lSelect, &nM, mH, &nM, vl, &ldvl, vr, &ldvr, &nM, &m, work, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);

            basic_srmatrix<float>& v = bRightVect ? vr : vl;
            const tint ldv = bRightVect ? ldvr : ldvl;

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            SGEBAK(Chars::pB(), 1, pRL, 1,
#else
            SGEBAK(Chars::pB(),    pRL,
#endif
                   &nM, &ilo, &ihi, vScale, &nM, v, &ldv, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);

            bool bPair = false;
            m = 0;
            mEigVect -> resize(nM);
            for (tint i = 0; i < nM; i++) {
                if (std::abs(vI(i)) > basic_cvmMachMin<float>()) {
                    (*mEigVect)(i).assign_real(v(m));

                    if (bPair) {
                        (*mEigVect)(i).assign_imag(- v(m + 1));
                        m += 2;
                        bPair = false;
                    } else {
                        (*mEigVect)(i).assign_imag(v(m + 1));
                        bPair = true;
                    }
                } else {
                    const float zero(0.F);
                    (*mEigVect)(i).assign_real(v(m));
                    (*mEigVect)(i).set_imag(zero);
                    m++;
                }
            }
        }
        else
        {
            lWork = -1;
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            SHSEQR(Chars::pE(), 1, Chars::pN(), 1,
#else
            SHSEQR(Chars::pE(),    Chars::pN(),
#endif
                   &nM, &ilo, &ihi, mA, &nM, vR, vI, nullptr, &nM, &dWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            lWork = static_cast<tint>(dWork) * 11;
            if (lWork > vWork.size()) vWork.resize(lWork);

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            SHSEQR(Chars::pE(), 1, Chars::pN(), 1,
#else
            SHSEQR(Chars::pE(),    Chars::pN(),
#endif
                   &nM, &ilo, &ihi, mA, &nM, vR, vI, nullptr, &nM, vWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "SHSEQR", __FILE__, __LINE__);
        }

        vRes.assign_real(vR);
        vRes.assign_imag(vI);
    }
}

template<>
CVM_API void
__eig<basic_cvector<double, std::complex<double>> , basic_srmatrix<double>, basic_scmatrix<double, std::complex<double>>  >
    (basic_cvector<double, std::complex<double>> & vRes,
    const basic_srmatrix<double>& mArg,
    basic_scmatrix<double, std::complex<double>> * mEigVect,
    bool bRightVect)
{
    const bool bEigVect = (mEigVect != nullptr);
    const tint nM = mArg.msize();
    tint lWork = -1;
    tint ilo = 0;
    tint ihi = 0;
    tint nOutInfo = 0;

    _check_ne(CVM_SIZESMISMATCH, vRes.size(), nM);
    if (nM == 1) {
        vRes[0] = std::complex<double>(mArg(0,0), 0.);
        if (bEigVect) {
            const std::complex<double> one(1., 0.);
            mEigVect -> resize(1);
            (*mEigVect)[0].set(one);
        }
    } else {
        basic_srmatrix<double> mA(mArg);
        basic_rvector<double> vScale(nM);
        basic_rvector<double> vTau(_cvm_max<tint>(1, nM - 1));
        basic_rvector<double> vR(nM);
        basic_rvector<double> vI(nM);

        DGEBAL(Chars::pB(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
               1,
#endif
               &nM, mA, &nM, &ilo, &ihi, vScale, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        double dWork;
        DGEHRD(&nM, &ilo, &ihi, mA, &nM, vTau, &dWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);
        lWork = static_cast<tint>(dWork);
        basic_rvector<double> vWork(lWork);

        DGEHRD(&nM, &ilo, &ihi, mA, &nM, vTau, vWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        if (bEigVect) {
            const tint one(1);
            tint m = 0;
            tint lSelect = 0;
            const tint ldvl = bRightVect ? 1 : nM;
            const tint ldvr = bRightVect ? nM : 1;
            basic_srmatrix<double> vl(ldvl);
            basic_srmatrix<double> vr(ldvr);
            basic_rvector <double> work(3 * nM);
            basic_srmatrix<double> mH(mA);
            const char* pRL = bRightVect ? Chars::pR() : Chars::pL();

            if (bRightVect) {
                vr = mA;
            } else {
                vl = mA;
            }

            lWork = -1;
            DORGHR(&nM, &one, &nM, bRightVect ? vr : vl, &nM, vTau, &dWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            lWork = static_cast<tint>(dWork);
            if (lWork > vWork.size()) vWork.resize(lWork);

            DORGHR(&nM, &one, &nM, bRightVect ? vr : vl, &nM, vTau, vWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);

            lWork = -1;
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            DHSEQR(Chars::pS(), 1, Chars::pV(), 1,
#else
            DHSEQR(Chars::pS(),    Chars::pV(),
#endif
                   &nM, &ilo, &ihi, mH, &nM, vR, vI, bRightVect ? vr : vl, &nM, &dWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            lWork = static_cast<tint>(dWork) * 11;
            if (lWork > vWork.size()) vWork.resize(lWork);

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            DHSEQR(Chars::pS(), 1, Chars::pV(), 1,
#else
            DHSEQR(Chars::pS(),    Chars::pV(),
#endif
                   &nM, &ilo, &ihi, mH, &nM, vR, vI, bRightVect ? vr : vl, &nM, vWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "DHSEQR", __FILE__, __LINE__);

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            DTREVC(pRL, 1, Chars::pB(), 1,
#else
            DTREVC(pRL,    Chars::pB(),
#endif
                   &lSelect, &nM, mH, &nM, vl, &ldvl, vr, &ldvr, &nM, &m, work, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);

            basic_srmatrix<double>& v = bRightVect ? vr   : vl;
            const tint ldv = bRightVect ? ldvr : ldvl;

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            DGEBAK(Chars::pB(), 1, pRL, 1,
#else
            DGEBAK(Chars::pB(),    pRL,
#endif
                   &nM, &ilo, &ihi, vScale, &nM, v, &ldv, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);

            bool bPair = false;
            m = 0;
            mEigVect -> resize(nM);
            for (tint i = 0; i < nM; i++) {
                if (std::abs(vI(i)) > basic_cvmMachMin<double>()) {
                    (*mEigVect)(i).assign_real(v(m));

                    if (bPair) {
                        (*mEigVect)(i).assign_imag(- v(m + 1));
                        m += 2;
                        bPair = false;
                    } else {
                        (*mEigVect)(i).assign_imag(v(m + 1));
                        bPair = true;
                    }
                } else {
                    const double zero(0.);
                    (*mEigVect)(i).assign_real(v(m));
                    (*mEigVect)(i).set_imag(zero);
                    m++;
                }
            }
        }
        else
        {
            lWork = -1;
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            DHSEQR(Chars::pE(), 1, Chars::pN(), 1,
#else
            DHSEQR(Chars::pE(), Chars::pN(),
#endif
                   &nM, &ilo, &ihi, mA, &nM, vR, vI, nullptr, &nM, &dWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            lWork = static_cast<tint>(dWork) * 11;
            if (lWork > vWork.size()) vWork.resize(lWork);

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            DHSEQR(Chars::pE(), 1, Chars::pN(), 1,
#else
            DHSEQR(Chars::pE(), Chars::pN(),
#endif
                   &nM, &ilo, &ihi, mA, &nM, vR, vI, nullptr, &nM, vWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "DHSEQR", __FILE__, __LINE__);
        }

        vRes.assign_real(vR);
        vRes.assign_imag(vI);
    }
}

template<>
CVM_API void
__eig<basic_cvector<float, std::complex<float>> , basic_scmatrix<float, std::complex<float>> , basic_scmatrix<float, std::complex<float>>  >
    (basic_cvector<float, std::complex<float>> & vRes,
    const basic_scmatrix<float, std::complex<float>> & mArg,
    basic_scmatrix<float, std::complex<float>> * mEigVect,
    bool bRightVect)
{
    const bool bEigVect = (mEigVect != nullptr);
    const tint nM = mArg.msize();
    tint lWork = -1;
    tint ilo = 0;
    tint ihi = 0;
    tint nOutInfo = 0;

    _check_ne(CVM_SIZESMISMATCH, vRes.size(), nM);
    if (nM == 1) {
        vRes[0] = mArg(0,0);
        if (bEigVect) {
            const std::complex<float> one(1.F, 0.F);
            mEigVect -> resize(1);
            (*mEigVect)[0].set(one);
        }
    }
    else
    {
        basic_scmatrix<float, std::complex<float>>  mA(mArg);
        basic_rvector<float> vScale(nM);
        basic_cvector<float, std::complex<float>>  vTau(_cvm_max<tint>(1, nM - 1));
        basic_cvector<float, std::complex<float>>  vW(nM);

        CGEBAL(Chars::pB(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
               1,
#endif
               &nM, mA, &nM, &ilo, &ihi, vScale, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        std::complex<float> dWork;
        CGEHRD(&nM, &ilo, &ihi, mA, &nM, vTau, &dWork, &lWork, &nOutInfo);
        lWork = static_cast<tint>(dWork.real());
        basic_cvector<float, std::complex<float>>  vWork(lWork);

        CGEHRD(&nM, &ilo, &ihi, mA, &nM, vTau, vWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        if (bEigVect) {
            const tint one(1);
            tint m = 0;
            tint lSelect = 0;
            const tint ldvl = bRightVect ? 1 : nM;
            const tint ldvr = bRightVect ? nM : 1;
            basic_scmatrix<float, std::complex<float>>  vl(ldvl);
            basic_scmatrix<float, std::complex<float>>  vr(ldvr);
            basic_cvector <float, std::complex<float>>  work(2 * nM);
            basic_rvector <float> rwork(nM);
            const char* pRL = bRightVect ? Chars::pR() : Chars::pL();

            if (bRightVect) {
                vr = mA;
            } else {
                vl = mA;
            }

            lWork = -1;
            CUNGHR(&nM, &one, &nM, bRightVect ? vr : vl, &nM, vTau, &dWork, &lWork, &nOutInfo);
            lWork = static_cast<tint>(dWork.real());
            if (lWork > vWork.size()) vWork.resize(lWork);

            CUNGHR(&nM, &one, &nM, bRightVect ? vr : vl, &nM, vTau, vWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);

            lWork = -1;
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            CHSEQR(Chars::pS(), 1, Chars::pV(), 1,
#else
            CHSEQR(Chars::pS(),    Chars::pV(),
#endif
                   &nM, &ilo, &ihi, mA, &nM, vW, bRightVect ? vr : vl, &nM, &dWork, &lWork, &nOutInfo);
            lWork = static_cast<tint> (dWork.real()) * 11;
            if (lWork > vWork.size()) vWork.resize(lWork);

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            CHSEQR(Chars::pS(), 1, Chars::pV(), 1,
#else
            CHSEQR(Chars::pS(),    Chars::pV(),
#endif
                   &nM, &ilo, &ihi, mA, &nM, vW, bRightVect ? vr : vl, &nM, vWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "CHSEQR", __FILE__, __LINE__);

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            CTREVC(pRL, 1, Chars::pB(), 1,
#else
            CTREVC(pRL,    Chars::pB(),
#endif
                   &lSelect, &nM, mA, &nM, vl, &ldvl, vr, &ldvr, &nM, &m, work, rwork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);

            basic_scmatrix<float, std::complex<float>> & v = bRightVect ? vr : vl;
            const tint ldv = bRightVect ? ldvr : ldvl;

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            CGEBAK(Chars::pB(), 1, pRL, 1,
#else
            CGEBAK(Chars::pB(),    pRL,
#endif
                   &nM, &ilo, &ihi, vScale, &nM, v, &ldv, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);

            (*mEigVect) << v;
        }
        else
        {
            lWork = -1;
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            CHSEQR(Chars::pE(), 1, Chars::pN(), 1,
#else
            CHSEQR(Chars::pE(),    Chars::pN(),
#endif
                   &nM, &ilo, &ihi, mA, &nM, vW, nullptr, &nM, &dWork, &lWork, &nOutInfo);
            lWork = static_cast<tint> (dWork.real()) * 11;
            if(lWork > vWork.size()) vWork.resize(lWork);

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            CHSEQR(Chars::pE(), 1, Chars::pN(), 1,
#else
            CHSEQR(Chars::pE(),    Chars::pN(),
#endif
                   &nM, &ilo, &ihi, mA, &nM, vW, nullptr, &nM, vWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "CHSEQR", __FILE__, __LINE__);
        }

        vRes.assign(vW, vW.incr());
    }
}

template<>
CVM_API void
__eig<basic_cvector<double, std::complex<double>> , basic_scmatrix<double, std::complex<double>> , basic_scmatrix<double, std::complex<double>>  >
    (basic_cvector<double, std::complex<double>> & vRes,
    const basic_scmatrix<double, std::complex<double>> & mArg,
    basic_scmatrix<double, std::complex<double>> * mEigVect,
    bool bRightVect)
{
    const bool bEigVect = (mEigVect != nullptr);
    const tint nM = mArg.msize();
    tint lWork = -1;
    tint ilo = 0;
    tint ihi = 0;
    tint nOutInfo = 0;

    _check_ne(CVM_SIZESMISMATCH, vRes.size(), nM);
    if (nM == 1) {
        vRes[0] = mArg(0,0);
        if (bEigVect) {
            const std::complex<double> one(1., 0.);
            mEigVect -> resize(1);
            (*mEigVect)[0].set(one);
        }
    }
    else
    {
        basic_scmatrix<double, std::complex<double>>  mA(mArg);
        basic_rvector<double> vScale(nM);
        basic_cvector<double, std::complex<double>>  vTau(_cvm_max<tint>(1, nM - 1));
        basic_cvector<double, std::complex<double>>  vW(nM);

        ZGEBAL(Chars::pB(),
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
               1,
#endif
               &nM, mA, &nM, &ilo, &ihi, vScale, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        std::complex<double> dWork;
        ZGEHRD(&nM, &ilo, &ihi, mA, &nM, vTau, &dWork, &lWork, &nOutInfo);
        lWork = static_cast<tint> (dWork.real());
        basic_cvector<double, std::complex<double>>  vWork(lWork);

        ZGEHRD(&nM, &ilo, &ihi, mA, &nM, vTau, vWork, &lWork, &nOutInfo);
        _check_negative(CVM_WRONGMKLARG, nOutInfo);

        if (bEigVect) {
            const tint one(1);
            tint m = 0;
            tint lSelect = 0;
            const tint ldvl = bRightVect ? 1 : nM;
            const tint ldvr = bRightVect ? nM : 1;
            basic_scmatrix<double, std::complex<double>>  vl(ldvl);
            basic_scmatrix<double, std::complex<double>>  vr(ldvr);
            basic_cvector <double, std::complex<double>>  work(2 * nM);
            basic_rvector <double> rwork(nM);
            const char* pRL = bRightVect ? Chars::pR() : Chars::pL();

            if (bRightVect) {
                vr = mA;
            } else {
                vl = mA;
            }

            lWork = -1;
            ZUNGHR(&nM, &one, &nM, bRightVect ? vr : vl, &nM, vTau, &dWork, &lWork, &nOutInfo);
            lWork = static_cast<tint> (dWork.real());
            if (lWork > vWork.size()) vWork.resize(lWork);

            ZUNGHR(&nM, &one, &nM, bRightVect ? vr : vl, &nM, vTau, vWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);

            lWork = -1;
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            ZHSEQR(Chars::pS(), 1, Chars::pV(), 1,
#else
            ZHSEQR(Chars::pS(),    Chars::pV(),
#endif
                   &nM, &ilo, &ihi, mA, &nM, vW, bRightVect ? vr : vl, &nM, &dWork, &lWork, &nOutInfo);
            lWork = static_cast<tint> (dWork.real()) * 11;
            if (lWork > vWork.size()) vWork.resize(lWork);

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            ZHSEQR(Chars::pS(), 1, Chars::pV(), 1,
#else
            ZHSEQR(Chars::pS(),    Chars::pV(),
#endif
                   &nM, &ilo, &ihi, mA, &nM, vW, bRightVect ? vr : vl, &nM, vWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "ZHSEQR", __FILE__, __LINE__);

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            ZTREVC(pRL, 1, Chars::pB(), 1,
#else
            ZTREVC(pRL,    Chars::pB(),
#endif
                   &lSelect, &nM, mA, &nM, vl, &ldvl, vr, &ldvr, &nM, &m, work, rwork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);

            basic_scmatrix<double, std::complex<double>> & v = bRightVect ? vr : vl;
            const tint ldv = bRightVect ? ldvr : ldvl;

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            ZGEBAK(Chars::pB(), 1, pRL, 1,
#else
            ZGEBAK(Chars::pB(),    pRL,
#endif
                   &nM, &ilo, &ihi, vScale, &nM, v, &ldv, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);

            (*mEigVect) << v;
        }
        else
        {
            lWork = -1;
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            ZHSEQR(Chars::pE(), 1, Chars::pN(), 1,
#else
            ZHSEQR(Chars::pE(),    Chars::pN(),
#endif
                   &nM, &ilo, &ihi, mA, &nM, vW, nullptr, &nM, &dWork, &lWork, &nOutInfo);
            lWork = static_cast<tint> (dWork.real()) * 11;
            if (lWork > vWork.size()) vWork.resize(lWork);

#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
            ZHSEQR(Chars::pE(), 1, Chars::pN(), 1,
#else
            ZHSEQR(Chars::pE(),    Chars::pN(),
#endif
                   &nM, &ilo, &ihi, mA, &nM, vW, nullptr, &nM, vWork, &lWork, &nOutInfo);
            _check_negative(CVM_WRONGMKLARG, nOutInfo);
            _check_positive(CVM_CONVERGENCE_ERROR, nOutInfo, "ZHSEQR", __FILE__, __LINE__);
        }

        vRes.assign(vW, vW.incr());
    }
}

// internal helper
void _harvest_eig_vect(basic_scmatrix<float, std::complex<float>> & mEigVect,
    const basic_srmatrix<float>& v,
    const basic_rvector<float>& alphai)
{
    float zero(0.F);
    for (tint i = 0; i < v.nsize(); ++i) {
        mEigVect(i).assign_real(v(i));
        if (i + 1 < v.nsize() && alphai(i) > basic_cvmMachMin<treal>()) {    // (i, i+1) form conjugate pair
            mEigVect(i + 1).assign_real(v(i));
            mEigVect(i).assign_imag(v(i + 1));
            mEigVect(i + 1).assign_imag(-v(i + 1));
            ++i;
            continue;
        }
        mEigVect(i).set_imag(zero);
    }
}

void _harvest_eig_vect(basic_scmatrix<double, std::complex<double>> & mEigVect,
    const basic_srmatrix<double>& v,
    const basic_rvector<double>& alphai)
{
    double zero(0.);
    for (tint i = 0; i < v.nsize(); ++i) {
        mEigVect(i).assign_real(v(i));
        if (i + 1 < v.nsize() && alphai(i) > basic_cvmMachMin<treal>()) {    // (i, i+1) form conjugate pair
            mEigVect(i + 1).assign_real(v(i));
            mEigVect(i).assign_imag(v(i + 1));
            mEigVect(i + 1).assign_imag(-v(i + 1));
            ++i;
            continue;
        }
        mEigVect(i).set_imag(zero);
    }
}

// Computes the generalized eigenvalues, and the
// left and / or right generalized eigenvectors for a pair
// of nonsymmetric matrices. 
// Overrides A and B!
template<>
CVM_API void
__ggev<basic_srmatrix<float>, basic_scmatrix<float, std::complex<float>> ,
basic_rvector<float>, basic_cvector<float, std::complex<float>>  >
(basic_srmatrix<float>& mA,
basic_srmatrix<float>& mB,
basic_cvector<float, std::complex<float>> & vAlpha,
basic_rvector<float>& vBeta,
basic_scmatrix<float, std::complex<float>> * mEigVectLeft,
basic_scmatrix<float, std::complex<float>> * mEigVectRight)
{
    const char* jobvl = mEigVectLeft == nullptr ? Chars::pN() : Chars::pV();
    const char* jobvr = mEigVectRight == nullptr ? Chars::pN() : Chars::pV();
    const tint n = mA.nsize();     // assert equal sizes outside
    basic_rvector<float> alphar(n);
    basic_rvector<float> alphai(n);
    basic_srmatrix<float> vl(n);
    basic_srmatrix<float> vr(n);
    tint lWork = -1;
    float dWork;
    tint nOutInfo = 0;

    SGGEV(jobvl,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          jobvr,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          &n, mA, mA._pld(), mB, mB._pld(),
          alphar, alphai, vBeta,
          vl, vl._pld(), vr, vr._pld(),
          &dWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    lWork = static_cast<tint>(dWork);
    basic_rvector<float> vWork(lWork);
    SGGEV(jobvl,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          jobvr,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          &n, mA, mA._pld(), mB, mB._pld(),
          alphar, alphai, vBeta,
          vl, vl._pld(), vr, vr._pld(),
          vWork, vWork._psize(), &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    if (mEigVectLeft != nullptr) {
        _harvest_eig_vect(*mEigVectLeft, vl, alphai);
    }
    if (mEigVectRight != nullptr) {
        _harvest_eig_vect(*mEigVectRight, vr, alphai);
    }
    vAlpha = basic_cvector<float, std::complex<float>> (alphar, alphai);
}

                    
template<>
CVM_API void
__ggev<basic_srmatrix<double>, basic_scmatrix<double, std::complex<double>> ,
       basic_rvector<double>, basic_cvector<double, std::complex<double>>  >
      (basic_srmatrix<double>& mA,
       basic_srmatrix<double>& mB,
       basic_cvector<double, std::complex<double>> & vAlpha,
       basic_rvector<double>& vBeta,
       basic_scmatrix<double, std::complex<double>> * mEigVectLeft,
       basic_scmatrix<double, std::complex<double>> * mEigVectRight)
{
    const char* jobvl = mEigVectLeft == nullptr ? Chars::pN() : Chars::pV();
    const char* jobvr = mEigVectRight == nullptr ? Chars::pN() : Chars::pV();
    const tint n = mA.nsize();     // assert equal sizes outside
    basic_rvector<double> alphar(n);
    basic_rvector<double> alphai(n);
    basic_srmatrix<double> vl(n);
    basic_srmatrix<double> vr(n);
    tint lWork = -1;
    double dWork;
    tint nOutInfo = 0;

    DGGEV(jobvl,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          jobvr,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          &n, mA, mA._pld(), mB, mB._pld(),
          alphar, alphai, vBeta,
          vl, vl._pld(), vr, vr._pld(),
          &dWork, &lWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    lWork = static_cast<tint>(dWork);
    basic_rvector<double> vWork(lWork);
    DGGEV(jobvl,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          jobvr,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          &n, mA, mA._pld(), mB, mB._pld(),
          alphar, alphai, vBeta,
          vl, vl._pld(), vr, vr._pld(),
          vWork, vWork._psize(), &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    if (mEigVectLeft != nullptr) {
        _harvest_eig_vect(*mEigVectLeft, vl, alphai);
    }
    if (mEigVectRight != nullptr) {
        _harvest_eig_vect(*mEigVectRight, vr, alphai);
    }
    vAlpha = basic_cvector<double, std::complex<double>> (alphar, alphai);
}

template<>
CVM_API void
__ggev<basic_scmatrix<float, std::complex<float>> , basic_scmatrix<float, std::complex<float>> ,
       basic_cvector<float, std::complex<float>> , basic_cvector<float, std::complex<float>>  >
      (basic_scmatrix<float, std::complex<float>> & mA,
       basic_scmatrix<float, std::complex<float>> & mB,
       basic_cvector<float, std::complex<float>> & vAlpha,
       basic_cvector<float, std::complex<float>> & vBeta,
       basic_scmatrix<float, std::complex<float>> * mEigVectLeft,
       basic_scmatrix<float, std::complex<float>> * mEigVectRight)
{
    const char* jobvl = mEigVectLeft == nullptr ? Chars::pN() : Chars::pV();
    const char* jobvr = mEigVectRight == nullptr ? Chars::pN() : Chars::pV();
    const tint n = mA.nsize();     // assert equal sizes outside
    basic_scmatrix<float, std::complex<float>>  vl(n);
    basic_scmatrix<float, std::complex<float>>  vr(n);
    basic_rvector<float> vRWork(8 * n);
    tint lWork = -1;
    std::complex<float> dWork;
    tint nOutInfo = 0;

    CGGEV(jobvl,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          jobvr,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          &n, mA, mA._pld(), mB, mB._pld(),
          vAlpha, vBeta,
          vl, vl._pld(), vr, vr._pld(),
          &dWork, &lWork, vRWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    lWork = static_cast<tint>(dWork.real());
    basic_cvector<float, std::complex<float>>  vWork(lWork);
    CGGEV(jobvl,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          jobvr,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          &n, mA, mA._pld(), mB, mB._pld(),
          vAlpha, vBeta,
          vl, vl._pld(), vr, vr._pld(),
          vWork, vWork._psize(), vRWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    if (mEigVectLeft != nullptr) {
        *mEigVectLeft = vl;
    }
    if (mEigVectRight != nullptr) {
        *mEigVectRight = vr;
    }
}

template<>
CVM_API void
__ggev<basic_scmatrix<double, std::complex<double>> , basic_scmatrix<double, std::complex<double>> ,
       basic_cvector<double, std::complex<double>> , basic_cvector<double, std::complex<double>>  >
      (basic_scmatrix<double, std::complex<double>> & mA,
       basic_scmatrix<double, std::complex<double>> & mB,
       basic_cvector<double, std::complex<double>> & vAlpha,
       basic_cvector<double, std::complex<double>> & vBeta,
       basic_scmatrix<double, std::complex<double>> * mEigVectLeft,
       basic_scmatrix<double, std::complex<double>> * mEigVectRight)
{
    const char* jobvl = mEigVectLeft == nullptr ? Chars::pN() : Chars::pV();
    const char* jobvr = mEigVectRight == nullptr ? Chars::pN() : Chars::pV();
    const tint n = mA.nsize();     // assert equal sizes outside
    basic_scmatrix<double, std::complex<double>>  vl(n);
    basic_scmatrix<double, std::complex<double>>  vr(n);
    basic_rvector<double> vRWork(8 * n);
    tint lWork = -1;
    std::complex<double> dWork;
    tint nOutInfo = 0;

    ZGGEV(jobvl,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          jobvr,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          &n, mA, mA._pld(), mB, mB._pld(),
          vAlpha, vBeta,
          vl, vl._pld(), vr, vr._pld(),
          &dWork, &lWork, vRWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    lWork = static_cast<tint>(dWork.real());
    basic_cvector<double, std::complex<double>>  vWork(lWork);
    ZGGEV(jobvl,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          jobvr,
#if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
          1,
#endif
          &n, mA, mA._pld(), mB, mB._pld(),
          vAlpha, vBeta,
          vl, vl._pld(), vr, vr._pld(),
          vWork, vWork._psize(), vRWork, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);

    if (mEigVectLeft != nullptr) {
        *mEigVectLeft = vl;
    }
    if (mEigVectRight != nullptr) {
        *mEigVectRight = vr;
    }
}

//! @endcond

CVM_NAMESPACE_END
