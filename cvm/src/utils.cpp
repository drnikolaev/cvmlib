//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2016
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "cvm.h"
#include "blas.h"
#include "cfun.h"

CVM_NAMESPACE_BEG

#if defined (CVM_STD_MUTEX)
std::mutex cvm_mutex;
#else
CriticalSection emCS;
#endif

//! @cond INTERNAL
char Chars::mchars[15] = { 'T', 'N', 'U', 'L', 'P', 'Q', 'B', 'E', 'R', 'A', 'S', 'V', 'O', 'I', 'C' };
//! @endcond

// global error messages holder
CVM_API ErrMessages& ErrMessages::ErrMessagesInstance()
{
    static ErrMessages _ErrMessages;
    return _ErrMessages;
}

CVM_API ErrMessages::ErrMessages()
    : msUnknown("Unknown exception"), mmMsg()
{
#if defined (CVM_STD_MUTEX)
    std::unique_lock<std::mutex> l(cvm_mutex);
#else
    Lock l(emCS);
#endif

    mmMsg.insert(pair_Msg(CVM_OK, "All OK"));
    mmMsg.insert(pair_Msg(CVM_OUTOFMEMORY, "Failed to allocate %u bytes of memory"));
    mmMsg.insert(pair_Msg(CVM_WRONGSIZE, "Wrong size: " CVM_TINT_FORMAT));
    mmMsg.insert(pair_Msg(CVM_SIZESMISMATCH, "Sizes mismatch: " CVM_TINT_FORMAT " != " CVM_TINT_FORMAT));
    mmMsg.insert(pair_Msg(CVM_WRONGMKLARG, "Wrong argument " CVM_TINT_FORMAT " passed to BLAS or LAPACK subroutine"));
    mmMsg.insert(pair_Msg(CVM_WRONGMKLARG2, "Wrong argument " CVM_TINT_FORMAT " passed to BLAS or LAPACK subroutine %s"));
    mmMsg.insert(pair_Msg(CVM_SINGULARMATRIX, "The diagonal element (or main minor) " CVM_TINT_FORMAT " of the matrix is zero (or singular)"));
    mmMsg.insert(pair_Msg(CVM_NOTPOSITIVEDEFINITE, "The leading minor of order " CVM_TINT_FORMAT " (hence the matrix itself) is not positive-definite"));
    mmMsg.insert(pair_Msg(CVM_WRONGCHOLESKYFACTOR, "The diagonal element " CVM_TINT_FORMAT " of the Cholesky factor (hence the factor itself) is zero"));
    mmMsg.insert(pair_Msg(CVM_WRONGBUNCHKAUFMANFACTOR, "The diagonal element " CVM_TINT_FORMAT " of the Bunch-Kaufman factor (and hence the factor itself) is zero"));
    mmMsg.insert(pair_Msg(CVM_NOTPOSITIVEDIAG, "The diagonal element " CVM_TINT_FORMAT " of the matrix is nonpositive. Equilibration failed"));
    mmMsg.insert(pair_Msg(CVM_CONVERGENCE_ERROR, "Method failed to converge: %s at %s:%d"));
    mmMsg.insert(pair_Msg(CVM_DIVISIONBYZERO, "Attempt to divide by zero"));
#if defined (WIN32) || defined (_WIN32)
    mmMsg.insert(pair_Msg(CVM_SEMAPHOREERROR, "Critical Section access error"));
#else
    mmMsg.insert(pair_Msg(CVM_SEMAPHOREERROR, "Semaphore access error"));
#endif
    mmMsg.insert(pair_Msg(CVM_READ_ONLY_ACCESS, "Attempt to change a read-only element"));
    mmMsg.insert(pair_Msg(CVM_SUBMATRIXACCESSERROR, "Attempt to access non-continuous submatrix as a continuous array, see programmer\'s reference for details"));
    mmMsg.insert(pair_Msg(CVM_SUBMATRIXNOTAVAILABLE, "Submatrix instantiation is not available for class \'%s\', see programmer\'s reference for details"));
    mmMsg.insert(pair_Msg(CVM_MATRIXNOTSYMMETRIC, "The matrix passed doesn't appear to be symmetric"));
    mmMsg.insert(pair_Msg(CVM_MATRIXNOTHERMITIAN, "The matrix passed doesn't appear to be hermitian (%g vs. tolerance %g)"));
    mmMsg.insert(pair_Msg(CVM_BREAKS_HERMITIANITY, "This operation could make the matrix non-hermitian. Use %s instead"));
    mmMsg.insert(pair_Msg(CVM_METHODNOTAVAILABLE, "Function \'%s\' is not available for class \'%s\'. See programmer\'s reference for details"));
    mmMsg.insert(pair_Msg(CVM_NOTIMPLEMENTED, "Function \'%s\' is not implemented"));
    mmMsg.insert(pair_Msg(CVM_CANT_RESIZE_SHARED_MEM, "Can\'t resize shared memory"));
    mmMsg.insert(pair_Msg(CVM_NOT_CONJUGATED, "Complex numbers are not conjugated: (%g,%g) vs. (%g,%g) with tolerance %g"));
    // 8.1
    mmMsg.insert(pair_Msg(CVM_WRONGSIZE_LT, "Wrong size: " CVM_TINT_FORMAT " < " CVM_TINT_FORMAT));
    mmMsg.insert(pair_Msg(CVM_WRONGSIZE_LE, "Wrong size: " CVM_TINT_FORMAT " <= " CVM_TINT_FORMAT));
    mmMsg.insert(pair_Msg(CVM_INDEX_GT, "Index value " CVM_TINT_FORMAT " > " CVM_TINT_FORMAT));
    mmMsg.insert(pair_Msg(CVM_INDEX_GE, "Index value " CVM_TINT_FORMAT " >= " CVM_TINT_FORMAT));
    mmMsg.insert(pair_Msg(CVM_OUTOFRANGE_LTGT, "Index value " CVM_TINT_FORMAT " is out of [" CVM_TINT_FORMAT "," CVM_TINT_FORMAT "] range"));
    mmMsg.insert(pair_Msg(CVM_OUTOFRANGE_LTGE, "Index value " CVM_TINT_FORMAT " is out of [" CVM_TINT_FORMAT "," CVM_TINT_FORMAT ") range"));
    mmMsg.insert(pair_Msg(CVM_OUTOFRANGE_LTGE1, "First index value " CVM_TINT_FORMAT " is out of [" CVM_TINT_FORMAT "," CVM_TINT_FORMAT ") range"));
    mmMsg.insert(pair_Msg(CVM_OUTOFRANGE_LTGE2, "Second index value " CVM_TINT_FORMAT " is out of [" CVM_TINT_FORMAT "," CVM_TINT_FORMAT ") range"));
    mmMsg.insert(pair_Msg(CVM_SIZESMISMATCH_GT, "Sizes mismatch: " CVM_TINT_FORMAT " > " CVM_TINT_FORMAT));
    mmMsg.insert(pair_Msg(CVM_SIZESMISMATCH_LT, "Sizes mismatch: " CVM_TINT_FORMAT " < " CVM_TINT_FORMAT));

    mmMsg.insert(pair_Msg(CFUN_PARSEERROR, "Error while parsing \'%s\' for variables %s"));
    mmMsg.insert(pair_Msg(CFUN_DOMAINERROR, "Domain error while calculating %s of %g"));
    mmMsg.insert(pair_Msg(CFUN_DOMAINERROR_C, "Domain error while calculating %s of (%g,%g)"));
    mmMsg.insert(pair_Msg(CFUN_CONVERGENCEERROR, "Convergence error while calculating %s of %g"));
    mmMsg.insert(pair_Msg(CFUN_CONVERGENCEERROR_C, "Convergence error while calculating %s of (%g,%g)"));
    mmMsg.insert(pair_Msg(CFUN_SUBSTPARAMETERERROR, "Error while substituting parameter \'%s\'"));
    mmMsg.insert(pair_Msg(CFUN_VARSDONTMATCH, "Variables don\'t match: \'%s\' vs. \'%s\'"));
    mmMsg.insert(pair_Msg(CFUN_NULLPOINTERERROR, "Null pointer passed to \'%s\'"));
    mmMsg.insert(pair_Msg(CFUN_PARAMETER_RECURSION, "Parameter \'%s\' can\'t be a part of its own meaning \'%s\'"));
}

CVM_API bool ErrMessages::_add(int nNewCause, const char* szNewMessage)
{
#if defined (CVM_STD_MUTEX)
    std::unique_lock<std::mutex> l(cvm_mutex);
#else
    Lock l(emCS);
#endif
    bool bRes = true;
    itr_Msg i = mmMsg.find(nNewCause);
    if (i != mmMsg.end()) {
        i->second = i->second + " | " + szNewMessage;      // Defenition is overlapped. This is not a good idea
        bRes = false;                                      // to do so, use CVM_THE_LAST_ERROR_CODE + 1 as an error code.
    }
    else {
        mmMsg.insert(pair_Msg(nNewCause, szNewMessage));   // new error definition
    }
    return bRes;
}

template <>
CVM_API float _real<std::complex<float>, float>(const std::complex<float>& mT)
{
    return mT.real();
}

template <>
CVM_API double _real<std::complex<double>, double>(const std::complex<double>& mT)
{
    return mT.real();
}

template <>
CVM_API float _imag<std::complex<float>, float>(const std::complex<float>& mT)
{
    return mT.imag();
}

template <>
CVM_API double _imag<std::complex<double>, double>(const std::complex<double>& mT)
{
    return mT.imag();
}

template <>
CVM_API void __copy<float>(tint nSize, const float* pFrom, tint nFromIncr, float* pTo, tint nToIncr)
{
    CVM_ASSERT(pFrom, ((nFromIncr)*(nSize - 1)+ 1)* sizeof(float))
    CVM_ASSERT(pTo, ((nToIncr)*(nSize - 1)+ 1)* sizeof(float))
    SCOPY(& nSize, pFrom, &nFromIncr, pTo, &nToIncr);
}

template <>
CVM_API void __copy<double>(tint nSize, const double* pFrom, tint nFromIncr, double* pTo, tint nToIncr)
{
    CVM_ASSERT(pFrom, ((nFromIncr)*(nSize - 1)+ 1)* sizeof(double))
    CVM_ASSERT(pTo, ((nToIncr)*(nSize - 1)+ 1)* sizeof(double))
    DCOPY(& nSize, pFrom, &nFromIncr, pTo, &nToIncr);
}

template <>
CVM_API void __copy<std::complex<float> >(tint nSize, const std::complex<float>* pFrom, tint nFromIncr, std::complex<float>* pTo, tint nToIncr)
{
    CVM_ASSERT(pFrom, ((nFromIncr)*(nSize - 1)+ 1)* sizeof(std::complex<float>))
    CVM_ASSERT(pTo, ((nToIncr)*(nSize - 1)+ 1)* sizeof(std::complex<float>))
    CCOPY(& nSize, pFrom, &nFromIncr, pTo, &nToIncr);
}

template <>
CVM_API void __copy<std::complex<double> >(tint nSize, const std::complex<double>* pFrom, tint nFromIncr, std::complex<double>* pTo, tint nToIncr)
{
    CVM_ASSERT(pFrom, ((nFromIncr)*(nSize - 1)+ 1)* sizeof(std::complex<double>))
    CVM_ASSERT(pTo, ((nToIncr)*(nSize - 1)+ 1)* sizeof(std::complex<double>))
    ZCOPY(& nSize, pFrom, &nFromIncr, pTo, &nToIncr);
}

template <>
CVM_API void __copy<tint>(tint nSize, const tint* pFrom, tint nFromIncr, tint* pTo, tint nToIncr)
{
    CVM_ASSERT(pFrom, ((nFromIncr)*(nSize - 1)+ 1)* sizeof(tint))
    CVM_ASSERT(pTo, ((nToIncr)*(nSize - 1)+ 1)* sizeof(tint))
    for(tint i = 0; i < nSize; ++i) {
            pTo[i* nToIncr] = pFrom[i* nFromIncr];
    }
}

template <>
CVM_API void __swap<float>(tint nSize, float* p1, tint n1Incr, float* p2, tint n2Incr)
{
    CVM_ASSERT(p1, ((n1Incr)*(nSize - 1)+ 1)* sizeof(float))
    CVM_ASSERT(p2, ((n2Incr)*(nSize - 1)+ 1)* sizeof(float))
    SSWAP(& nSize, p1, &n1Incr, p2, &n2Incr);
}

template <>
CVM_API void __swap<double>(tint nSize, double* p1, tint n1Incr, double* p2, tint n2Incr)
{
    CVM_ASSERT(p1, ((n1Incr)*(nSize - 1)+ 1)* sizeof(double))
    CVM_ASSERT(p2, ((n2Incr)*(nSize - 1)+ 1)* sizeof(double))
    DSWAP(& nSize, p1, &n1Incr, p2, &n2Incr);
}

template <>
CVM_API void __swap<std::complex<float> >(tint nSize, std::complex<float>* p1, tint n1Incr, std::complex<float>* p2, tint n2Incr)
{
    CVM_ASSERT(p1, ((n1Incr)*(nSize - 1)+ 1)* sizeof(std::complex<float>))
    CVM_ASSERT(p2, ((n2Incr)*(nSize - 1)+ 1)* sizeof(std::complex<float>))
    CSWAP(& nSize, p1, &n1Incr, p2, &n2Incr);
}

template <>
CVM_API void __swap<std::complex<double> >(tint nSize, std::complex<double>* p1, tint n1Incr, std::complex<double>* p2, tint n2Incr)
{
    CVM_ASSERT(p1, ((n1Incr)*(nSize - 1)+ 1)* sizeof(std::complex<double>))
    CVM_ASSERT(p2, ((n2Incr)*(nSize - 1)+ 1)* sizeof(std::complex<double>))
    ZSWAP(& nSize, p1, &n1Incr, p2, &n2Incr);
}

template <>
CVM_API void __swap<tint>(tint nSize, tint* p1, tint n1Incr, tint* p2, tint n2Incr)
{
    tint n;
    CVM_ASSERT(p1, (n1Incr*(nSize - 1)+ 1)* sizeof(tint))
    CVM_ASSERT(p2, (n2Incr*(nSize - 1)+ 1)* sizeof(tint))
    for(tint i = 0; i < nSize; ++i) {
        n = p1[i* n1Incr];
        p1[i* n1Incr] = p2[i* n2Incr];
        p2[i* n2Incr] = n;
    }
}

template <>
CVM_API void __low_up<basic_srmatrix<float> >(basic_srmatrix<float>& m, tint* nPivots)throw(cvmexception)
{
    tint nOutInfo = 0;
    SGETRF(m._pm(), m._pn(), m, m._pld(), nPivots, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_SINGULARMATRIX, nOutInfo);
}

template <>
CVM_API void __low_up<basic_srmatrix<double> >(basic_srmatrix<double>& m, tint* nPivots)throw(cvmexception)
{
    tint nOutInfo = 0;
    DGETRF(m._pm(), m._pn(), m, m._pld(), nPivots, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_SINGULARMATRIX, nOutInfo);
}

template <>
CVM_API void __low_up<basic_scmatrix<float, std::complex<float> > >
    (basic_scmatrix<float, std::complex<float> >& m, tint* nPivots)throw(cvmexception)
{
    tint nOutInfo = 0;
    CGETRF(m._pm(), m._pn(), m, m._pld(), nPivots, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_SINGULARMATRIX, nOutInfo);
}

template <>
CVM_API void __low_up<basic_scmatrix<double, std::complex<double> > >
    (basic_scmatrix<double, std::complex<double> >& m, tint* nPivots)throw(cvmexception)
{
    tint nOutInfo = 0;
    ZGETRF(m._pm(), m._pn(), m, m._pld(), nPivots, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_SINGULARMATRIX, nOutInfo);
}

template <>
CVM_API void __low_up<basic_srbmatrix<float> >
    (basic_srbmatrix<float>& m, tint* nPivots)throw(cvmexception)
{
    tint nOutInfo = 0;
    const tint nKL = m.lsize();
    const tint nKU = m.usize();
    m.resize_lu(nKL, nKL + nKU);
    SGBTRF(m._pm(), m._pn(), &nKL, &nKU, m, m._pld(), nPivots, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_SINGULARMATRIX, nOutInfo);
}

template <>
CVM_API void __low_up<basic_srbmatrix<double> >
    (basic_srbmatrix<double>& m, tint* nPivots)throw(cvmexception)
{
    tint nOutInfo = 0;
    const tint nKL = m.lsize();
    const tint nKU = m.usize();
    m.resize_lu(nKL, nKL + nKU);
    DGBTRF(m._pm(), m._pn(), &nKL, &nKU, m, m._pld(), nPivots, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_SINGULARMATRIX, nOutInfo);
}

template <>
CVM_API void __low_up<basic_scbmatrix<float, std::complex<float> > >
    (basic_scbmatrix<float, std::complex<float> >& m, tint* nPivots)throw(cvmexception)
{
    tint nOutInfo = 0;
    const tint nKL = m.lsize();
    const tint nKU = m.usize();
    m.resize_lu(nKL, nKL + nKU);
    CGBTRF(m._pm(), m._pn(), &nKL, &nKU, m, m._pld(), nPivots, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_SINGULARMATRIX, nOutInfo);
}

template <>
CVM_API void __low_up<basic_scbmatrix<double, std::complex<double> > >
    (basic_scbmatrix<double, std::complex<double> >& m, tint* nPivots)throw(cvmexception)
{
    tint nOutInfo = 0;
    const tint nKL = m.lsize();
    const tint nKU = m.usize();
    m.resize_lu(nKL, nKL + nKU);
    ZGBTRF(m._pm(), m._pn(), &nKL, &nKU, m, m._pld(), nPivots, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_SINGULARMATRIX, nOutInfo);
}

template <>
CVM_API tint __cholesky<basic_srmatrix<float> >
    (basic_srmatrix<float>& m) // input is symmetric, output is triangular
{
    tint nOutInfo = 0;
    SPOTRF(Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), m, m._pld(), &nOutInfo);
    return nOutInfo;
}

template <>
CVM_API tint __cholesky<basic_srmatrix<double> >
    (basic_srmatrix<double>& m) // input is symmetric, output is triangular
{
    tint nOutInfo = 0;
    DPOTRF(Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), m, m._pld(), &nOutInfo);
    return nOutInfo;
}

template <>
CVM_API tint __cholesky<basic_scmatrix<float, std::complex<float> > >
    (basic_scmatrix<float, std::complex<float> >& m) // input is hermitian, output is triangular
{
    tint nOutInfo = 0;
    CPOTRF(Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), m, m._pld(), &nOutInfo);
    return nOutInfo;
}

template <>
CVM_API tint __cholesky<basic_scmatrix<double, std::complex<double> > >
    (basic_scmatrix<double, std::complex<double> >& m) // input is hermitian, output is triangular
{
    tint nOutInfo = 0;
    ZPOTRF(Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), m, m._pld(), &nOutInfo);
    return nOutInfo;
}

template <>
CVM_API void __bunch_kaufman<basic_srmatrix<float> >
    (basic_srmatrix<float>& m, tint* nPivots)throw(cvmexception) // input is symmetric, output is square
{
    tint nOutInfo = 0;
    const tint lwork = m.msize()* 64;
    basic_rvector<float> work(lwork);
    SSYTRF(Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), m, m._pld(), nPivots, work, &lwork, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_SINGULARMATRIX, nOutInfo);
}

template <>
CVM_API void __bunch_kaufman<basic_srmatrix<double> >
    (basic_srmatrix<double>& m, tint* nPivots)throw(cvmexception) // input is symmetric, output is square
{
    tint nOutInfo = 0;
    const tint lwork = m.msize()* 64;
    basic_rvector<double> work(lwork);
    DSYTRF(Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), m, m._pld(), nPivots, work, &lwork, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_SINGULARMATRIX, nOutInfo);
}

template <>
CVM_API void __bunch_kaufman<basic_scmatrix<float, std::complex<float> > >
    (basic_scmatrix<float, std::complex<float> >& m, tint* nPivots)throw(cvmexception) // input is hermitian, output is square
{
    tint nOutInfo = 0;
    const tint lwork = m.msize()* 64;
    basic_cvector<float, std::complex<float> > work(lwork);
    CHETRF(Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), m, m._pld(), nPivots, work, &lwork, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_SINGULARMATRIX, nOutInfo);
}

template <>
CVM_API void __bunch_kaufman<basic_scmatrix<double, std::complex<double> > >
    (basic_scmatrix<double, std::complex<double> >& m, tint* nPivots)throw(cvmexception) // input is hermitian, output is square
{
    tint nOutInfo = 0;
    const tint lwork = m.msize()* 64;
    basic_cvector<double, std::complex<double> > work(lwork);
    ZHETRF(Chars::pU(),
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
           1,
#endif
           m._pm(), m, m._pld(), nPivots, work, &lwork, &nOutInfo);

    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_SINGULARMATRIX, nOutInfo);
}

template <>
CVM_API void __ger<float, basic_rmatrix<float>, basic_rvector<float> >
    (basic_rmatrix<float>& m,
     const basic_rvector<float>& vCol,
     const basic_rvector<float>& vRow,
     float dAlpha)
{
    CVM_ASSERT(m.get(), vCol.size()* vRow.size()* sizeof(float))
    SGER(vCol._psize(), vRow._psize(), &dAlpha, vCol, vCol._pincr(), vRow, vRow._pincr(), m, m._pld());
}

template <>
CVM_API void __ger<double, basic_rmatrix<double>, basic_rvector<double> >
    (basic_rmatrix<double>& m,
     const basic_rvector<double>& vCol,
     const basic_rvector<double>& vRow,
     double dAlpha)
{
    CVM_ASSERT(m.get(), vCol.size()* vRow.size()* sizeof(double))
    DGER(vCol._psize(), vRow._psize(), &dAlpha, vCol, vCol._pincr(), vRow, vRow._pincr(), m, m._pld());
}

//! @cond SPECIALIZATIONS
template <>
CVM_API void __geru<std::complex<float>, basic_cmatrix<float, std::complex<float> >, basic_cvector<float, std::complex<float> > >
    (basic_cmatrix<float, std::complex<float> >& m,
     const basic_cvector<float, std::complex<float> >& vCol,
     const basic_cvector<float, std::complex<float> >& vRow,
     std::complex<float> cAlpha)
{
    CVM_ASSERT(m, vCol.size()* vRow.size()* sizeof(std::complex<float>))
    CGERU(vCol._psize(), vRow._psize(), &cAlpha, vCol, vCol._pincr(), vRow, vRow._pincr(), m, m._pld());
}

template <>
CVM_API void __geru<std::complex<double>, basic_cmatrix<double, std::complex<double> >, basic_cvector<double, std::complex<double> > >
    (basic_cmatrix<double, std::complex<double> >& m,
     const basic_cvector<double, std::complex<double> >& vCol,
     const basic_cvector<double, std::complex<double> >& vRow,
     std::complex<double> cAlpha)
{
    CVM_ASSERT(m, vCol.size()* vRow.size()* sizeof(std::complex<double>))
    ZGERU(vCol._psize(), vRow._psize(), &cAlpha, vCol, vCol._pincr(), vRow, vRow._pincr(), m, m._pld());
}

template <>
CVM_API void __gerc<std::complex<float>, basic_cmatrix<float, std::complex<float> >, basic_cvector<float, std::complex<float> > >
    (basic_cmatrix<float, std::complex<float> >& m,
     const basic_cvector<float, std::complex<float> >& vCol,
     const basic_cvector<float, std::complex<float> >& vRow,
     std::complex<float> cAlpha)
{
    CVM_ASSERT(m, vCol.size()* vRow.size()* sizeof(std::complex<float>))
    CGERC(vCol._psize(), vRow._psize(), &cAlpha, vCol, vCol._pincr(), vRow, vRow._pincr(), m, m._pld());
}

template <>
CVM_API void __gerc<std::complex<double>, basic_cmatrix<double, std::complex<double> >, basic_cvector<double, std::complex<double> > >
    (basic_cmatrix<double, std::complex<double> >& m,
     const basic_cvector<double, std::complex<double> >& vCol,
     const basic_cvector<double, std::complex<double> >& vRow,
     std::complex<double> cAlpha)
{
    CVM_ASSERT(m, vCol.size()* vRow.size()* sizeof(std::complex<double>))
    ZGERC(vCol._psize(), vRow._psize(), &cAlpha, vCol, vCol._pincr(), vRow, vRow._pincr(), m, m._pld());
}
//! @endcond

template <>
CVM_API void __poequ<float, basic_srsmatrix<float>, basic_rvector<float> >
    (const basic_srsmatrix<float>& m,
     basic_rvector<float>& vScalings,
     float& dCond,
     float& dMax)
{
    tint nOutInfo = 0;
    SPOEQU(m._pm(), m, m._pld(), vScalings, &dCond, &dMax, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_NOTPOSITIVEDIAG, nOutInfo);
}

template <>
CVM_API void __poequ<double, basic_srsmatrix<double>, basic_rvector<double> >
    (const basic_srsmatrix<double>& m,
     basic_rvector<double>& vScalings,
     double& dCond,
     double& dMax)
{
    tint nOutInfo = 0;
    DPOEQU(m._pm(), m, m._pld(), vScalings, &dCond, &dMax, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_NOTPOSITIVEDIAG, nOutInfo);
}

template <>
CVM_API void __poequ<float, basic_schmatrix<float, std::complex<float> >, basic_rvector<float> >
    (const basic_schmatrix<float, std::complex<float> >& m,
     basic_rvector<float>& vScalings,
     float& dCond,
     float& dMax)
{
    tint nOutInfo = 0;
    CPOEQU(m._pm(), m, m._pld(), vScalings, &dCond, &dMax, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_NOTPOSITIVEDIAG, nOutInfo);
}

template <>
CVM_API void __poequ<double, basic_schmatrix<double, std::complex<double> >, basic_rvector<double> >
    (const basic_schmatrix<double, std::complex<double> >& m,
     basic_rvector<double>& vScalings,
     double& dCond,
     double& dMax)
{
    tint nOutInfo = 0;
    ZPOEQU(m._pm(), m, m._pld(), vScalings, &dCond, &dMax, &nOutInfo);
    _check_negative(CVM_WRONGMKLARG, nOutInfo);
    _check_positive(CVM_NOTPOSITIVEDIAG, nOutInfo);
}

CVM_NAMESPACE_END
