//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2016
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "cvm.h"
#include "blas.h"
#include <cmath>

CVM_NAMESPACE_BEG

// 8.0: tint-based spec due to Array->basic_array refactoring
template<>
CVM_API tint __norm<tint, tint>(const tint* pd, tint nSize, tint nIncr) {
    CVM_ASSERT(pd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(tint))
    rvector rv(nSize);
    for (tint i = 0; i < nSize; ++i) {
        rv[CVM0+i] = static_cast<treal>(pd[i*nIncr]);
    }
    return static_cast<tint>(floor(rv.norm() + 0.5));
}

// 8.0: tint-based spec due to Array->basic_array refactoring
template<>
CVM_API void __scal<tint, tint>(tint* pd, tint nSize, tint nIncr, tint dScal) {
    CVM_ASSERT(pd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(tint))
    for (tint i = 0; i < nSize; ++i) {
        pd[i*nIncr] *= dScal;
    }
}

// 8.0: tint-based spec due to Array->basic_array refactoring
template<>
CVM_API tint __idamax<tint>(const tint* pd, tint nSize, tint nIncr) {
    CVM_ASSERT(pd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(tint))
    tint ret = 0, max = - (std::numeric_limits<tint>::max)(), val;
    for (tint i = 0; i < nSize; ++i) {
        val = pd[i*nIncr];
        if (val > max) {
            max = val;
            ret = i;
        }
    }
    return ret + CVM0;
}

// 8.0: tint-based spec due to Array->basic_array refactoring
template<>
CVM_API tint __idamin<tint>(const tint* pd, tint nSize, tint nIncr) {
    CVM_ASSERT(pd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(tint))
    tint ret = 0, min = (std::numeric_limits<tint>::max)(), val;
    for (tint i = 0; i < nSize; ++i) {
        val = pd[i*nIncr];
        if (val < min) {
            min = val;
            ret = i;
        }
    }
    return ret + CVM0;
}


template<>
CVM_API float __norm<float, float>(const float* mpd, tint nSize, tint nIncr) {
    CVM_ASSERT(mpd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(float))
    return SNRM2(&nSize, mpd, &nIncr);
}

template<>
CVM_API double __norm<double, double>(const double* mpd, tint nSize, tint nIncr) {
    CVM_ASSERT(mpd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(double))
    return DNRM2(&nSize, mpd, &nIncr);
}

template<>
CVM_API float __norm<float, std::complex<float> >(const std::complex<float>* mpd,
                                                  tint nSize, tint nIncr) {
    CVM_ASSERT(mpd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(std::complex<float>))
    return SCNRM2(&nSize, mpd, &nIncr);
}

template<>
CVM_API double __norm<double, std::complex<double> >(const std::complex<double>* mpd,
                                                     tint nSize, tint nIncr) {
    CVM_ASSERT(mpd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(std::complex<double>))
    return DZNRM2(&nSize, mpd, &nIncr);
}

template<>
CVM_API tint __idamax<float>(const float* mpd, tint nSize, tint nIncr) {
    CVM_ASSERT(mpd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(float))
    return ISAMAX(&nSize, mpd, &nIncr) - (1 - CVM0);
}

template<>
CVM_API tint __idamax<double>(const double* mpd, tint nSize, tint nIncr) {
    CVM_ASSERT(mpd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(double))
    return IDAMAX(&nSize, mpd, &nIncr) - (1 - CVM0);
}

template<>
CVM_API tint __idamin<float>(const float* mpd, tint nSize, tint nIncr) {
    CVM_ASSERT(mpd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(float))
    return ISAMIN(&nSize, mpd, &nIncr) - (1 - CVM0);
}

template<>
CVM_API tint __idamin<double>(const double* mpd, tint nSize, tint nIncr) {
    CVM_ASSERT(mpd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(double))
    return IDAMIN(&nSize, mpd, &nIncr) - (1 - CVM0);
}

template<>
CVM_API tint __idamax<std::complex<float> >(const std::complex<float>* mpd,
                                            tint nSize, tint nIncr) {
    CVM_ASSERT(mpd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(std::complex<float>))
    return ICAMAX(&nSize, mpd, &nIncr) - (1 - CVM0);
}

template<>
CVM_API tint __idamax<std::complex<double> >(const std::complex<double>* mpd,
                                             tint nSize, tint nIncr) {
    CVM_ASSERT(mpd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(std::complex<double>))
    return IZAMAX(&nSize, mpd, &nIncr) - (1 - CVM0);
}

template<>
CVM_API tint __idamin<std::complex<float> >(const std::complex<float>* mpd,
                                            tint nSize, tint nIncr) {
    CVM_ASSERT(mpd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(std::complex<float>))
    return ICAMIN(&nSize, mpd, &nIncr) - (1 - CVM0);
}

template<>
CVM_API tint __idamin<std::complex<double> >(const std::complex<double>* mpd,
                                             tint nSize, tint nIncr) {
    CVM_ASSERT(mpd, ((nSize - CVM0) * nIncr + CVM0) * sizeof(std::complex<double>))
    return IZAMIN(&nSize, mpd, &nIncr) - (1 - CVM0);
}

template<>
CVM_API void __add<float>(float* mpd, tint mnSize, tint mnIncr,
                          const float* pv, tint nIncr) {
    static const float one(1.F);
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(float))
    CVM_ASSERT(pv, ((mnSize - CVM0) * nIncr + CVM0) * sizeof(float))
    SAXPY(&mnSize, &one, pv, &nIncr, mpd, &mnIncr);
}

template<>
CVM_API void __add<double>(double* mpd, tint mnSize, tint mnIncr,
                           const double* pv, tint nIncr) {
    static const double one(1.);
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(double))
    CVM_ASSERT(pv, ((mnSize - CVM0) * nIncr + CVM0) * sizeof(double))
    DAXPY(&mnSize, &one, pv, &nIncr, mpd, &mnIncr);
}

template<>
CVM_API void __subtract<float>(float* mpd, tint mnSize, tint mnIncr,
                               const float* pv, tint nIncr) {
    static const float mone(-1.F);
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(float))
    CVM_ASSERT(pv, ((mnSize - CVM0) * nIncr + CVM0) * sizeof(float))
    SAXPY(&mnSize, &mone, pv, &nIncr, mpd, &mnIncr);
}

template<>
CVM_API void __subtract<double>(double* mpd, tint mnSize, tint mnIncr,
                                const double* pv, tint nIncr) {
    static const double mone(-1.F);
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(double))
    CVM_ASSERT(pv, ((mnSize - CVM0) * nIncr + CVM0) * sizeof(double))
    DAXPY(&mnSize, &mone, pv, &nIncr, mpd, &mnIncr);
}

template<>
CVM_API void __add<std::complex<float> >(std::complex<float>* mpd, tint mnSize, tint mnIncr,
                                         const std::complex<float>* pv, tint nIncr) {
    static const std::complex<float> one(1.F, 0.F);
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(std::complex<float>))
    CVM_ASSERT(pv, ((mnSize - CVM0) * nIncr + CVM0) * sizeof(std::complex<float>))
    CAXPY(&mnSize, &one, pv, &nIncr, mpd, &mnIncr);
}

template<>
CVM_API void __add<std::complex<double> >(std::complex<double>* mpd, tint mnSize, tint mnIncr,
                                          const std::complex<double>* pv, tint nIncr) {
    static const std::complex<double> one(1., 0.);
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(std::complex<double>))
    CVM_ASSERT(pv, ((mnSize - CVM0) * nIncr + CVM0) * sizeof(std::complex<double>))
    ZAXPY(&mnSize, &one, pv, &nIncr, mpd, &mnIncr);
}

template<>
CVM_API void __subtract<std::complex<float> >(std::complex<float>* mpd, tint mnSize, tint mnIncr,
                                              const std::complex<float>* pv, tint nIncr) {
    static const std::complex<float> mone(-1.F, 0.F);
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(std::complex<float>))
    CVM_ASSERT(pv, ((mnSize - CVM0) * nIncr + CVM0) * sizeof(std::complex<float>))
    CAXPY(&mnSize, &mone, pv, &nIncr, mpd, &mnIncr);
}

template<>
CVM_API void __subtract<std::complex<double> >(std::complex<double>* mpd, tint mnSize, tint mnIncr,
                                               const std::complex<double>* pv, tint nIncr) {
    static const std::complex<double> mone(-1., 0.);
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(std::complex<double>))
    CVM_ASSERT(pv, ((mnSize - CVM0) * nIncr + CVM0) * sizeof(std::complex<double>))
    ZAXPY(&mnSize, &mone, pv, &nIncr, mpd, &mnIncr);
}

template<>
CVM_API void __scal<float, float>(float* mpd, tint mnSize, tint mnIncr, float dScal) {
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(float))
    SSCAL(&mnSize, &dScal, mpd, &mnIncr);
}

template<>
CVM_API void __scal<double, double>(double* mpd, tint mnSize, tint mnIncr, double dScal) {
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(double))
    DSCAL(&mnSize, &dScal, mpd, &mnIncr);
}

template<>
CVM_API void __scal<float, std::complex<float> >(std::complex<float>* mpd, tint mnSize,
                                                 tint mnIncr, float dScal) {
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(std::complex<float>))
    CSSCAL(&mnSize, &dScal, mpd, &mnIncr);
}

template<>
CVM_API void __scal<double, std::complex<double> >(std::complex<double>* mpd, tint mnSize,
                                                   tint mnIncr, double dScal) {
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(std::complex<double>))
    ZDSCAL(&mnSize, &dScal, mpd, &mnIncr);
}

//! @cond SPECIALIZATIONS
template<>
CVM_API void
__scal<std::complex<float>, std::complex<float> >(std::complex<float>* mpd, tint mnSize,
                                                  tint mnIncr, std::complex<float> cScal) {
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(std::complex<float>))
    CSCAL(&mnSize, &cScal, mpd, &mnIncr);
}

template<>
CVM_API void
__scal<std::complex<double>, std::complex<double> >(std::complex<double>* mpd, tint mnSize,
                                                    tint mnIncr, std::complex<double> cScal) {
    CVM_ASSERT(mpd, ((mnSize - CVM0) * mnIncr + CVM0) * sizeof(std::complex<double>))
    ZSCAL(&mnSize, &cScal, mpd, &mnIncr);
}
//! @endcond

template<>
CVM_API void
__copy2<float, std::complex<float> > (std::complex<float>* mpd, tint mnSize, tint mnIncr,
                                      const float* pRe, const float* pIm,
                                      tint nReIncr, tint nImIncr) {
    const tint nIncr2 = mnIncr * 2;
    float* pR = __get_real_p<float>(mpd);
    float* pI = __get_imag_p<float>(mpd);

    if (pRe) {
        CVM_ASSERT(pRe, ((mnSize - CVM0) * nReIncr + CVM0) * sizeof(float))
        CVM_ASSERT(pR, ((mnSize - CVM0) * nIncr2 + CVM0) * sizeof(float))
        SCOPY(&mnSize, pRe, &nReIncr, pR, &nIncr2);
    } else {
        static const float zero(0.F);
        CVM_ASSERT(pR, ((mnSize - CVM0) * nIncr2 + CVM0) * sizeof(float))
        SSCAL(&mnSize, &zero, pR, &nIncr2);
    }

    if (pIm) {
        CVM_ASSERT(pIm, ((mnSize - CVM0) * nImIncr + CVM0) * sizeof(float))
        CVM_ASSERT(pI, ((mnSize - CVM0) * nIncr2 + CVM0) * sizeof(float))
        SCOPY(&mnSize, pIm, &nImIncr, pI, &nIncr2);
    } else {
        static const float zero(0.F);
        CVM_ASSERT(pI, ((mnSize - CVM0) * nIncr2 + CVM0) * sizeof(float))
        SSCAL(&mnSize, &zero, pI, &nIncr2);
    }
}

template<>
CVM_API void
__copy2<double, std::complex<double> > (std::complex<double>* mpd, tint mnSize, tint mnIncr,
                                        const double* pRe, const double* pIm,
                                        tint nReIncr, tint nImIncr) {
    const tint nIncr2 = mnIncr * 2;
    double* pR = __get_real_p<double>(mpd);
    double* pI = __get_imag_p<double>(mpd);

    if (pRe) {
        CVM_ASSERT(pRe, ((mnSize - CVM0) * nReIncr + CVM0) * sizeof(double))
        CVM_ASSERT(pR, ((mnSize - CVM0) * nIncr2 + CVM0) * sizeof(double))
        DCOPY(&mnSize, pRe, &nReIncr, pR, &nIncr2);
    } else {
        static const double zero(0.);
        CVM_ASSERT(pR, ((mnSize - CVM0) * nIncr2 + CVM0) * sizeof(double))
        DSCAL(&mnSize, &zero, pR, &nIncr2);
    }

    if (pIm) {
        CVM_ASSERT(pIm, ((mnSize - CVM0) * nImIncr + CVM0) * sizeof(double))
        CVM_ASSERT(pI, ((mnSize - CVM0) * nIncr2 + CVM0) * sizeof(double))
        DCOPY(&mnSize, pIm, &nImIncr, pI, &nIncr2);
    } else {
        static const double zero(0.);
        CVM_ASSERT(pI, ((mnSize - CVM0) * nIncr2 + CVM0) * sizeof(double))
        DSCAL(&mnSize, &zero, pI, &nIncr2);
    }
}

template<>
CVM_API void
__copy_real<float, std::complex<float> > (std::complex<float>* mpd,
                                          tint mnSize, tint mnIncr,
                                          const float* pRe, tint nReIncr) {
    float* pdr = __get_real_p<float>(mpd);
    if (pdr != pRe) {
        const tint nIncr2 = mnIncr * 2;
        CVM_ASSERT(pRe, ((mnSize - CVM0) * nReIncr + CVM0) * sizeof(float))
        CVM_ASSERT(pdr, ((mnSize - CVM0) * nIncr2 + CVM0) * sizeof(float))
        SCOPY(&mnSize, pRe, &nReIncr, pdr, &nIncr2);
    }
}

template<>
CVM_API void
__copy_real<double, std::complex<double> > (std::complex<double>* mpd,
                                            tint mnSize, tint mnIncr,
                                            const double* pRe, tint nReIncr) {
    double* pdr = __get_real_p<double>(mpd);
    if (pdr != pRe) {
        const tint nIncr2 = mnIncr * 2;
        CVM_ASSERT(pRe, ((mnSize - CVM0) * nReIncr + CVM0) * sizeof(double))
        CVM_ASSERT(pdr, ((mnSize - CVM0) * nIncr2 + CVM0) * sizeof(double))
        DCOPY(&mnSize, pRe, &nReIncr, pdr, &nIncr2);
    }
}

template<>
CVM_API void
__copy_imag<float, std::complex<float> > (std::complex<float>* mpd,
                                          tint mnSize, tint mnIncr,
                                          const float* pIm, tint nImIncr) {
    float* pdi = __get_imag_p<float>(mpd);
    if (pdi != pIm) {
        const tint nIncr2 = mnIncr * 2;
        CVM_ASSERT(pIm, ((mnSize - CVM0) * nImIncr + CVM0) * sizeof(float))
        CVM_ASSERT(pdi, ((mnSize - CVM0) * nIncr2 + CVM0) * sizeof(float))
        SCOPY(&mnSize, pIm, &nImIncr, pdi, &nIncr2);
    }
}

template<>
CVM_API void
__copy_imag<double, std::complex<double> > (std::complex<double>* mpd,
                                            tint mnSize, tint mnIncr,
                                            const double* pIm, tint nImIncr) {
    double* pdi = __get_imag_p<double>(mpd);
    if (pdi != pIm) {
        const tint nIncr2 = mnIncr * 2;
        CVM_ASSERT(pIm, ((mnSize - CVM0) * nImIncr + CVM0) * sizeof(double))
        CVM_ASSERT(pdi, ((mnSize - CVM0) * nIncr2 + CVM0) * sizeof(double))
        DCOPY(&mnSize, pIm, &nImIncr, pdi, &nIncr2);
    }
}

template<>
CVM_API void __conj<std::complex<float> > (std::complex<float>* mpd, tint mnSize, tint mnIncr) {
    CLACGV(&mnSize, mpd, &mnIncr);
}

template<>
CVM_API void __conj<std::complex<double> > (std::complex<double>* mpd, tint mnSize, tint mnIncr) {
    ZLACGV(&mnSize, mpd, &mnIncr);
}

template<>
CVM_API void __randomize<float> (float* mpd, tint mnSize, tint mnIncr, float dFrom, float dTo) {
    const tint nSize = mnSize * mnIncr;
    for (tint i = 0; i < nSize; i += mnIncr) {
        mpd[i] = Randomizer<float>::get(dFrom, dTo);
    }
}

template<>
CVM_API void __randomize<double> (double* mpd, tint mnSize, tint mnIncr, double dFrom, double dTo) {
    const tint nSize = mnSize * mnIncr;
    for (tint i = 0; i < nSize; i += mnIncr) {
        mpd[i] = Randomizer<double>::get(dFrom, dTo);
    }
}

template<>
CVM_API void
__randomize_real<std::complex<float>, float> (std::complex<float>* mpd,
                                              tint mnSize, tint mnIncr,
                                              float dFrom, float dTo) {
    const tint nIncr = 2 * mnIncr;
    const tint nSize = mnSize * nIncr;
    float* pdr = __get_real_p<float>(mpd);

    for (tint i = 0; i < nSize; i += nIncr) {
        pdr[i] = Randomizer<float>::get(dFrom, dTo);
    }
}

template<>
CVM_API void
__randomize_real<std::complex<double>, double> (std::complex<double>* mpd,
                                                tint mnSize, tint mnIncr,
                                                double dFrom, double dTo) {
    const tint nIncr = 2 * mnIncr;
    const tint nSize = mnSize * nIncr;
    double* pdr = __get_real_p<double>(mpd);

    for (tint i = 0; i < nSize; i += nIncr) {
        pdr[i] = Randomizer<double>::get(dFrom, dTo);
    }
}

template<>
CVM_API void
__randomize_imag<std::complex<float>, float> (std::complex<float>* mpd,
                                              tint mnSize, tint mnIncr,
                                              float dFrom, float dTo) {
    const tint nIncr = 2 * mnIncr;
    const tint nSize = mnSize * nIncr;
    float* pdi = __get_imag_p<float>(mpd);

    for (tint i = 0; i < nSize; i += nIncr) {
        pdi[i] = Randomizer<float>::get(dFrom, dTo);
    }
}

template<>
CVM_API void
__randomize_imag<std::complex<double>, double> (std::complex<double>* mpd,
                                                tint mnSize, tint mnIncr,
                                                double dFrom, double dTo) {
    const tint nIncr = 2 * mnIncr;
    const tint nSize = mnSize * nIncr;
    double* pdi = __get_imag_p<double>(mpd);

    for (tint i = 0; i < nSize; i += nIncr) {
        pdi[i] = Randomizer<double>::get(dFrom, dTo);
    }
}

CVM_NAMESPACE_END

