//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2023
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "cvm.h"
#include "blas.h"
#include <cmath>

CVM_NAMESPACE_BEG

// 8.0: tint-based spec due to Array->basic_array refactoring
template<>
CVM_API tint __norm<tint, tint>(const tint* pd, tint size, tint incr) {
    CVM_ASSERT(pd, (size * incr) * sizeof(tint))
    rvector rv(size);
    for (tint i = 0; i < size; ++i) {
        rv[i] = static_cast<treal>(pd[i*incr]);
    }
    return static_cast<tint>(floor(rv.norm() + 0.5));
}

// 8.0: tint-based spec due to Array->basic_array refactoring
template<>
CVM_API void __scal<tint, tint>(tint* pd, tint size, tint incr, tint dScal) {
    CVM_ASSERT(pd, (size * incr) * sizeof(tint))
    for (tint i = 0; i < size; ++i) {
        pd[i*incr] *= dScal;
    }
}

// 8.0: tint-based spec due to Array->basic_array refactoring
template<>
CVM_API tint __idamax<tint>(const tint* pd, tint size, tint incr) {
    CVM_ASSERT(pd, (size * incr) * sizeof(tint))
    tint ret = 0, max = - (std::numeric_limits<tint>::max)(), val;
    for (tint i = 0; i < size; ++i) {
        val = pd[i*incr];
        if (val > max) {
            max = val;
            ret = i;
        }
    }
    return ret;
}

// 8.0: tint-based spec due to Array->basic_array refactoring
template<>
CVM_API tint __idamin<tint>(const tint* pd, tint size, tint incr) {
    CVM_ASSERT(pd, (size * incr) * sizeof(tint))
    tint ret = 0, min = (std::numeric_limits<tint>::max)(), val;
    for (tint i = 0; i < size; ++i) {
        val = pd[i*incr];
        if (val < min) {
            min = val;
            ret = i;
        }
    }
    return ret;
}


template<>
CVM_API float __norm<float, float>(const float* mpd, tint size, tint incr) {
    CVM_ASSERT(mpd, (size * incr) * sizeof(float))
    return SNRM2(&size, mpd, &incr);
}

template<>
CVM_API double __norm<double, double>(const double* mpd, tint size, tint incr) {
    CVM_ASSERT(mpd, (size * incr) * sizeof(double))
    return DNRM2(&size, mpd, &incr);
}

template<>
CVM_API float __norm<float, std::complex<float>> (const std::complex<float>* mpd,
                                                  tint size, tint incr) {
    CVM_ASSERT(mpd, (size * incr) * sizeof(std::complex<float>))
    return SCNRM2(&size, mpd, &incr);
}

template<>
CVM_API double __norm<double, std::complex<double>> (const std::complex<double>* mpd,
                                                     tint size, tint incr) {
    CVM_ASSERT(mpd, (size * incr) * sizeof(std::complex<double>))
    return DZNRM2(&size, mpd, &incr);
}

template<>
CVM_API tint __idamax<float>(const float* mpd, tint size, tint incr) {
    CVM_ASSERT(mpd, (size * incr) * sizeof(float))
    return ISAMAX(&size, mpd, &incr) - 1;
}

template<>
CVM_API tint __idamax<double>(const double* mpd, tint size, tint incr) {
    CVM_ASSERT(mpd, (size * incr) * sizeof(double))
    return IDAMAX(&size, mpd, &incr) - 1;
}

template<>
CVM_API tint __idamin<float>(const float* mpd, tint size, tint incr) {
    CVM_ASSERT(mpd, (size * incr) * sizeof(float))
    return ISAMIN(&size, mpd, &incr) - 1;
}

template<>
CVM_API tint __idamin<double>(const double* mpd, tint size, tint incr) {
    CVM_ASSERT(mpd, (size * incr) * sizeof(double))
    return IDAMIN(&size, mpd, &incr) - 1;
}

template<>
CVM_API tint __idamax<std::complex<float>> (const std::complex<float>* mpd,
                                            tint size, tint incr) {
    CVM_ASSERT(mpd, (size * incr) * sizeof(std::complex<float>))
    return ICAMAX(&size, mpd, &incr) - 1;
}

template<>
CVM_API tint __idamax<std::complex<double>> (const std::complex<double>* mpd,
                                             tint size, tint incr) {
    CVM_ASSERT(mpd, (size * incr) * sizeof(std::complex<double>))
    return IZAMAX(&size, mpd, &incr) - 1;
}

template<>
CVM_API tint __idamin<std::complex<float>> (const std::complex<float>* mpd,
                                            tint size, tint incr) {
    CVM_ASSERT(mpd, (size * incr) * sizeof(std::complex<float>))
    return ICAMIN(&size, mpd, &incr) - 1;
}

template<>
CVM_API tint __idamin<std::complex<double>> (const std::complex<double>* mpd,
                                             tint size, tint incr) {
    CVM_ASSERT(mpd, (size * incr) * sizeof(std::complex<double>))
    return IZAMIN(&size, mpd, &incr) - 1;
}

template<>
CVM_API void __add<float>(float* mpd, tint mn_size, tint mn_incr,
                          const float* pv, tint incr) {
    const float one(1.F);
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(float))
    CVM_ASSERT(pv, (mn_size * incr) * sizeof(float))
    SAXPY(&mn_size, &one, pv, &incr, mpd, &mn_incr);
}

template<>
CVM_API void __add<double>(double* mpd, tint mn_size, tint mn_incr,
                           const double* pv, tint incr) {
    const double one(1.);
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(double))
    CVM_ASSERT(pv, (mn_size * incr) * sizeof(double))
    DAXPY(&mn_size, &one, pv, &incr, mpd, &mn_incr);
}

template<>
CVM_API void __subtract<float>(float* mpd, tint mn_size, tint mn_incr,
                               const float* pv, tint incr) {
    const float mone(-1.F);
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(float))
    CVM_ASSERT(pv, (mn_size * incr) * sizeof(float))
    SAXPY(&mn_size, &mone, pv, &incr, mpd, &mn_incr);
}

template<>
CVM_API void __subtract<double>(double* mpd, tint mn_size, tint mn_incr,
                                const double* pv, tint incr) {
    const double mone(-1.F);
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(double))
    CVM_ASSERT(pv, (mn_size * incr) * sizeof(double))
    DAXPY(&mn_size, &mone, pv, &incr, mpd, &mn_incr);
}

template<>
CVM_API void __add<std::complex<float>> (std::complex<float>* mpd, tint mn_size, tint mn_incr,
                                         const std::complex<float>* pv, tint incr) {
    const std::complex<float> one(1.F, 0.F);
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(std::complex<float>))
    CVM_ASSERT(pv, (mn_size * incr) * sizeof(std::complex<float>))
    CAXPY(&mn_size, &one, pv, &incr, mpd, &mn_incr);
}

template<>
CVM_API void __add<std::complex<double>> (std::complex<double>* mpd, tint mn_size, tint mn_incr,
                                          const std::complex<double>* pv, tint incr) {
    const std::complex<double> one(1., 0.);
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(std::complex<double>))
    CVM_ASSERT(pv, (mn_size * incr) * sizeof(std::complex<double>))
    ZAXPY(&mn_size, &one, pv, &incr, mpd, &mn_incr);
}

template<>
CVM_API void __subtract<std::complex<float>> (std::complex<float>* mpd, tint mn_size, tint mn_incr,
                                              const std::complex<float>* pv, tint incr) {
    const std::complex<float> mone(-1.F, 0.F);
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(std::complex<float>))
    CVM_ASSERT(pv, (mn_size * incr) * sizeof(std::complex<float>))
    CAXPY(&mn_size, &mone, pv, &incr, mpd, &mn_incr);
}

template<>
CVM_API void __subtract<std::complex<double>> (std::complex<double>* mpd, tint mn_size, tint mn_incr,
                                               const std::complex<double>* pv, tint incr) {
    const std::complex<double> mone(-1., 0.);
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(std::complex<double>))
    CVM_ASSERT(pv, (mn_size * incr) * sizeof(std::complex<double>))
    ZAXPY(&mn_size, &mone, pv, &incr, mpd, &mn_incr);
}

template<>
CVM_API void __scal<float, float>(float* mpd, tint mn_size, tint mn_incr, float dScal) {
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(float))
    SSCAL(&mn_size, &dScal, mpd, &mn_incr);
}

template<>
CVM_API void __scal<double, double>(double* mpd, tint mn_size, tint mn_incr, double dScal) {
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(double))
    DSCAL(&mn_size, &dScal, mpd, &mn_incr);
}

template<>
CVM_API void __scal<float, std::complex<float>> (std::complex<float>* mpd, tint mn_size,
                                                 tint mn_incr, float dScal) {
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(std::complex<float>))
    CSSCAL(&mn_size, &dScal, mpd, &mn_incr);
}

template<>
CVM_API void __scal<double, std::complex<double>> (std::complex<double>* mpd, tint mn_size,
                                                   tint mn_incr, double dScal) {
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(std::complex<double>))
    ZDSCAL(&mn_size, &dScal, mpd, &mn_incr);
}

//! @cond SPECIALIZATIONS
template<>
CVM_API void
__scal<std::complex<float>, std::complex<float>> (std::complex<float>* mpd, tint mn_size,
                                                  tint mn_incr, std::complex<float> cScal) {
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(std::complex<float>))
    CSCAL(&mn_size, &cScal, mpd, &mn_incr);
}

template<>
CVM_API void
__scal<std::complex<double>, std::complex<double>> (std::complex<double>* mpd, tint mn_size,
                                                    tint mn_incr, std::complex<double> cScal) {
    CVM_ASSERT(mpd, (mn_size * mn_incr) * sizeof(std::complex<double>))
    ZSCAL(&mn_size, &cScal, mpd, &mn_incr);
}
//! @endcond

template<>
CVM_API void
__copy2<float, std::complex<float>>  (std::complex<float>* mpd, tint mn_size, tint mn_incr,
                                      const float* pRe, const float* pIm,
                                      tint re_incr, tint im_incr) {
    const tint incr2 = mn_incr * 2;
    auto* pR = __get_real_p<float>(mpd);
    auto* pI = __get_imag_p<float>(mpd);

    if (pRe) {
        CVM_ASSERT(pRe, (mn_size * re_incr) * sizeof(float))
        CVM_ASSERT(pR, (mn_size * incr2) * sizeof(float))
        SCOPY(&mn_size, pRe, &re_incr, pR, &incr2);
    } else {
        const float zero(0.F);
        CVM_ASSERT(pR, (mn_size * incr2) * sizeof(float))
        SSCAL(&mn_size, &zero, pR, &incr2);
    }

    if (pIm) {
        CVM_ASSERT(pIm, (mn_size * im_incr) * sizeof(float))
        CVM_ASSERT(pI, (mn_size * incr2) * sizeof(float))
        SCOPY(&mn_size, pIm, &im_incr, pI, &incr2);
    } else {
        const float zero(0.F);
        CVM_ASSERT(pI, (mn_size * incr2) * sizeof(float))
        SSCAL(&mn_size, &zero, pI, &incr2);
    }
}

template<>
CVM_API void
__copy2<double, std::complex<double>> (std::complex<double>* mpd, tint mn_size, tint mn_incr,
                                        const double* pRe, const double* pIm,
                                        tint re_incr, tint im_incr) {
    const tint incr2 = mn_incr * 2;
    auto* pR = __get_real_p<double>(mpd);
    auto* pI = __get_imag_p<double>(mpd);

    if (pRe) {
        CVM_ASSERT(pRe, (mn_size * re_incr) * sizeof(double))
        CVM_ASSERT(pR, (mn_size * incr2) * sizeof(double))
        DCOPY(&mn_size, pRe, &re_incr, pR, &incr2);
    } else {
        const double zero(0.);
        CVM_ASSERT(pR, (mn_size * incr2) * sizeof(double))
        DSCAL(&mn_size, &zero, pR, &incr2);
    }

    if (pIm) {
        CVM_ASSERT(pIm, (mn_size * im_incr) * sizeof(double))
        CVM_ASSERT(pI, (mn_size * incr2) * sizeof(double))
        DCOPY(&mn_size, pIm, &im_incr, pI, &incr2);
    } else {
        const double zero(0.);
        CVM_ASSERT(pI, (mn_size * incr2) * sizeof(double))
        DSCAL(&mn_size, &zero, pI, &incr2);
    }
}

template<>
CVM_API void
__copy_real<float, std::complex<float>> (std::complex<float>* mpd,
                                          tint mn_size, tint mn_incr,
                                          const float* pRe, tint re_incr) {
    auto* pdr = __get_real_p<float>(mpd);
    if (pdr != pRe) {
        const tint incr2 = mn_incr * 2;
        CVM_ASSERT(pRe, (mn_size * re_incr) * sizeof(float))
        CVM_ASSERT(pdr, (mn_size * incr2) * sizeof(float))
        SCOPY(&mn_size, pRe, &re_incr, pdr, &incr2);
    }
}

template<>
CVM_API void
__copy_real<double, std::complex<double>> (std::complex<double>* mpd,
                                            tint mn_size, tint mn_incr,
                                            const double* pRe, tint re_incr) {
    auto* pdr = __get_real_p<double>(mpd);
    if (pdr != pRe) {
        const tint incr2 = mn_incr * 2;
        CVM_ASSERT(pRe, (mn_size * re_incr) * sizeof(double))
        CVM_ASSERT(pdr, (mn_size * incr2) * sizeof(double))
        DCOPY(&mn_size, pRe, &re_incr, pdr, &incr2);
    }
}

template<>
CVM_API void
__copy_imag<float, std::complex<float>>  (std::complex<float>* mpd,
                                          tint mn_size, tint mn_incr,
                                          const float* pIm, tint im_incr) {
    auto* pdi = __get_imag_p<float>(mpd);
    if (pdi != pIm) {
        const tint incr2 = mn_incr * 2;
        CVM_ASSERT(pIm, (mn_size * im_incr) * sizeof(float))
        CVM_ASSERT(pdi, (mn_size * incr2) * sizeof(float))
        SCOPY(&mn_size, pIm, &im_incr, pdi, &incr2);
    }
}

template<>
CVM_API void
__copy_imag<double, std::complex<double>>  (std::complex<double>* mpd,
                                            tint mn_size, tint mn_incr,
                                            const double* pIm, tint im_incr) {
    auto* pdi = __get_imag_p<double>(mpd);
    if (pdi != pIm) {
        const tint incr2 = mn_incr * 2;
        CVM_ASSERT(pIm, (mn_size * im_incr) * sizeof(double))
        CVM_ASSERT(pdi, (mn_size * incr2) * sizeof(double))
        DCOPY(&mn_size, pIm, &im_incr, pdi, &incr2);
    }
}

template<>
CVM_API void __conj<std::complex<float>>  (std::complex<float>* mpd, tint mn_size, tint mn_incr) {
    CLACGV(&mn_size, mpd, &mn_incr);
}

template<>
CVM_API void __conj<std::complex<double>>  (std::complex<double>* mpd, tint mn_size, tint mn_incr) {
    ZLACGV(&mn_size, mpd, &mn_incr);
}

template<>
CVM_API void __randomize<float> (float* mpd, tint mn_size, tint mn_incr, float dFrom, float dTo) {
    const tint size = mn_size * mn_incr;
    for (tint i = 0; i < size; i += mn_incr) {
        mpd[i] = Randomizer<float>::get(dFrom, dTo);
    }
}

template<>
CVM_API void __randomize<double> (double* mpd, tint mn_size, tint mn_incr, double dFrom, double dTo) {
    const tint size = mn_size * mn_incr;
    for (tint i = 0; i < size; i += mn_incr) {
        mpd[i] = Randomizer<double>::get(dFrom, dTo);
    }
}

template<>
CVM_API void
__randomize_real<std::complex<float>, float> (std::complex<float>* mpd,
                                              tint mn_size, tint mn_incr,
                                              float dFrom, float dTo) {
    const tint incr = 2 * mn_incr;
    const tint size = mn_size * incr;
    auto* pdr = __get_real_p<float>(mpd);

    for (tint i = 0; i < size; i += incr) {
        pdr[i] = Randomizer<float>::get(dFrom, dTo);
    }
}

template<>
CVM_API void
__randomize_real<std::complex<double>, double> (std::complex<double>* mpd,
                                                tint mn_size, tint mn_incr,
                                                double dFrom, double dTo) {
    const tint incr = 2 * mn_incr;
    const tint size = mn_size * incr;
    auto* pdr = __get_real_p<double>(mpd);

    for (tint i = 0; i < size; i += incr) {
        pdr[i] = Randomizer<double>::get(dFrom, dTo);
    }
}

template<>
CVM_API void
__randomize_imag<std::complex<float>, float> (std::complex<float>* mpd,
                                              tint mn_size, tint mn_incr,
                                              float dFrom, float dTo) {
    const tint incr = 2 * mn_incr;
    const tint size = mn_size * incr;
    auto* pdi = __get_imag_p<float>(mpd);

    for (tint i = 0; i < size; i += incr) {
        pdi[i] = Randomizer<float>::get(dFrom, dTo);
    }
}

template<>
CVM_API void
__randomize_imag<std::complex<double>, double> (std::complex<double>* mpd,
                                                tint mn_size, tint mn_incr,
                                                double dFrom, double dTo) {
    const tint incr = 2 * mn_incr;
    const tint size = mn_size * incr;
    auto* pdi = __get_imag_p<double>(mpd);

    for (tint i = 0; i < size; i += incr) {
        pdi[i] = Randomizer<double>::get(dFrom, dTo);
    }
}

CVM_NAMESPACE_END
