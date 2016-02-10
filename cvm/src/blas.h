//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2014
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)


#ifndef _BLAS_H
#define _BLAS_H

#ifdef __cplusplus
extern "C" {
#endif

#if defined (_MSC_VER)
#    define CVM_FTN_CALL __stdcall
#    define CVM_STD_CALL __stdcall
#else
#    define CVM_FTN_CALL
#    define CVM_STD_CALL

// my fortran stuff

#    define DPOLY   dpoly_
#    define SPOLY   spoly_
#    define CPOLY   cpoly_
#    define ZPOLY   zpoly_

#    define NPOLY   npoly_

#    define SMEXP   smexp_
#    define DMEXP   dmexp_
#    define CMEXP   cmexp_
#    define ZMEXP   zmexp_

#    define SMEXPC  smexpc_
#    define DMEXPC  dmexpc_
#    define CMEXPC  cmexpc_
#    define ZMEXPC  zmexpc_

// blas & lapack stuff

#    define ISAMAX  isamax_
#    define IDAMAX  idamax_
#    define ISAMIN  isamin_
#    define IDAMIN  idamin_

#    define ICAMAX  icamax_
#    define IZAMAX  izamax_
#    define ICAMIN  icamin_
#    define IZAMIN  izamin_

#    define SNRM2   snrm2_
#    define DNRM2   dnrm2_
#    define SCNRM2  scnrm2_
#    define DZNRM2  dznrm2_

#    define SSWAP   sswap_
#    define DSWAP   dswap_
#    define CSWAP   cswap_
#    define ZSWAP   zswap_

#    define SDOT    sdot_
#    define DDOT    ddot_

// complex dot wrappers

#    if defined (CVM_COMPLEX_NUMBER_RETURNED)
#    define VCDOTU  cdotu_
#    define VZDOTU  zdotu_
#    define VCDOTC  cdotc_
#    define VZDOTC  zdotc_
#    else
#    define VCDOTU  vcdotu_
#    define VZDOTU  vzdotu_
#    define VCDOTC  vcdotc_
#    define VZDOTC  vzdotc_
#    endif

#    define SAXPY   saxpy_
#    define DAXPY   daxpy_
#    define CAXPY   caxpy_
#    define ZAXPY   zaxpy_

#    define SCOPY   scopy_
#    define DCOPY   dcopy_
#    define CCOPY   ccopy_
#    define ZCOPY   zcopy_

#    define SSCAL   sscal_
#    define DSCAL   dscal_
#    define CSCAL   cscal_
#    define ZSCAL   zscal_

#    define CLACGV  clacgv_
#    define ZLACGV  zlacgv_

#    define CSSCAL  csscal_
#    define ZDSCAL  zdscal_

#    define SGER    sger_
#    define DGER    dger_

#    define CGERU   cgeru_
#    define ZGERU   zgeru_

#    define CGERC   cgerc_
#    define ZGERC   zgerc_

#    define SGEMV   sgemv_
#    define DGEMV   dgemv_
#    define CGEMV   cgemv_
#    define ZGEMV   zgemv_

#    define SGBMV   sgbmv_
#    define DGBMV   dgbmv_
#    define CGBMV   cgbmv_
#    define ZGBMV   zgbmv_

#    define SGEMM   sgemm_
#    define DGEMM   dgemm_
#    define CGEMM   cgemm_
#    define ZGEMM   zgemm_

#    define SGETRF  sgetrf_
#    define DGETRF  dgetrf_
#    define CGETRF  cgetrf_
#    define ZGETRF  zgetrf_

#    define SGBTRF  sgbtrf_
#    define DGBTRF  dgbtrf_
#    define CGBTRF  cgbtrf_
#    define ZGBTRF  zgbtrf_

#    define SGETRS  sgetrs_
#    define DGETRS  dgetrs_
#    define CGETRS  cgetrs_
#    define ZGETRS  zgetrs_

#    define SGBTRS  sgbtrs_
#    define DGBTRS  dgbtrs_
#    define CGBTRS  cgbtrs_
#    define ZGBTRS  zgbtrs_

#    define SGERFS  sgerfs_
#    define DGERFS  dgerfs_
#    define CGERFS  cgerfs_
#    define ZGERFS  zgerfs_

#    define SGBRFS  sgbrfs_
#    define DGBRFS  dgbrfs_
#    define CGBRFS  cgbrfs_
#    define ZGBRFS  zgbrfs_

#    define SGETRI  sgetri_
#    define DGETRI  dgetri_
#    define CGETRI  cgetri_
#    define ZGETRI  zgetri_

#    define SGEBRD  sgebrd_
#    define DGEBRD  dgebrd_
#    define CGEBRD  cgebrd_
#    define ZGEBRD  zgebrd_

#    define SGBBRD  sgbbrd_
#    define DGBBRD  dgbbrd_
#    define CGBBRD  cgbbrd_
#    define ZGBBRD  zgbbrd_

#    define SORGBR  sorgbr_
#    define DORGBR  dorgbr_

#    define CUNGBR  cungbr_
#    define ZUNGBR  zungbr_

#    define SBDSQR  sbdsqr_
#    define DBDSQR  dbdsqr_
#    define CBDSQR  cbdsqr_
#    define ZBDSQR  zbdsqr_

#    define SGEBAL  sgebal_
#    define DGEBAL  dgebal_
#    define CGEBAL  cgebal_
#    define ZGEBAL  zgebal_

#    define SGEHRD  sgehrd_
#    define DGEHRD  dgehrd_
#    define CGEHRD  cgehrd_
#    define ZGEHRD  zgehrd_

#    define SORGHR  sorghr_
#    define DORGHR  dorghr_

#    define CUNGHR  cunghr_
#    define ZUNGHR  zunghr_

#    define SHSEQR  shseqr_
#    define DHSEQR  dhseqr_
#    define CHSEQR  chseqr_
#    define ZHSEQR  zhseqr_

#    define STREVC  strevc_
#    define DTREVC  dtrevc_
#    define CTREVC  ctrevc_
#    define ZTREVC  ztrevc_

#    define SGEBAK  sgebak_
#    define DGEBAK  dgebak_
#    define CGEBAK  cgebak_
#    define ZGEBAK  zgebak_

#    define SGECON  sgecon_
#    define DGECON  dgecon_
#    define CGECON  cgecon_
#    define ZGECON  zgecon_

#    define SSPMV   sspmv_
#    define DSPMV   dspmv_

#    define SSYMM   ssymm_
#    define DSYMM   dsymm_
#    define CSYMM   csymm_
#    define ZSYMM   zsymm_
#    define CHEMM   chemm_
#    define ZHEMM   zhemm_

#    define SPOTRF  spotrf_
#    define DPOTRF  dpotrf_
#    define CPOTRF  cpotrf_
#    define ZPOTRF  zpotrf_

#    define SSYTRF  ssytrf_
#    define DSYTRF  dsytrf_
#    define CSYTRF  csytrf_
#    define ZSYTRF  zsytrf_
#    define CHETRF  chetrf_
#    define ZHETRF  zhetrf_

#    define SPOTRS  spotrs_
#    define DPOTRS  dpotrs_
#    define CPOTRS  cpotrs_
#    define ZPOTRS  zpotrs_

#    define SPORFS  sporfs_
#    define DPORFS  dporfs_
#    define CPORFS  cporfs_
#    define ZPORFS  zporfs_

#    define SSYTRS  ssytrs_
#    define DSYTRS  dsytrs_
#    define CSYTRS  csytrs_
#    define ZSYTRS  zsytrs_
#    define CHETRS  chetrs_
#    define ZHETRS  zhetrs_

#    define SSYRFS  ssyrfs_
#    define DSYRFS  dsyrfs_
#    define CSYRFS  csyrfs_
#    define ZSYRFS  zsyrfs_
#    define CHERFS  cherfs_
#    define ZHERFS  zherfs_

#    define SPOTRI  spotri_
#    define DPOTRI  dpotri_
#    define CPOTRI  cpotri_
#    define ZPOTRI  zpotri_

#    define SSYTRI  ssytri_
#    define DSYTRI  dsytri_
#    define CSYTRI  csytri_
#    define ZSYTRI  zsytri_
#    define CHETRI  chetri_
#    define ZHETRI  zhetri_

#    define SSYEVD  ssyevd_
#    define DSYEVD  dsyevd_
#    define CHEEVD  cheevd_
#    define ZHEEVD  zheevd_

#    define SPOEQU  spoequ_
#    define DPOEQU  dpoequ_
#    define CPOEQU  cpoequ_
#    define ZPOEQU  zpoequ_

#    define SSYMV   ssymv_
#    define DSYMV   dsymv_
#    define CHEMV   chemv_
#    define ZHEMV   zhemv_

#    define SSYRK   ssyrk_
#    define DSYRK   dsyrk_
#    define CSYRK   csyrk_
#    define ZSYRK   zsyrk_
#    define CHERK   cherk_
#    define ZHERK   zherk_

#    define SSYR2K  ssyr2k_
#    define DSYR2K  dsyr2k_
#    define CSYR2K  csyr2k_
#    define ZSYR2K  zsyr2k_
#    define CHER2K  cher2k_
#    define ZHER2K  zher2k_

#    define SGEQRF  sgeqrf_
#    define DGEQRF  dgeqrf_
#    define CGEQRF  cgeqrf_
#    define ZGEQRF  zgeqrf_

#    define SORGQR  sorgqr_
#    define DORGQR  dorgqr_
#    define CUNGQR  cungqr_
#    define ZUNGQR  zungqr_

#    define SGERQF  sgerqf_
#    define DGERQF  dgerqf_
#    define CGERQF  cgerqf_
#    define ZGERQF  zgerqf_

#    define SORGRQ  sorgrq_
#    define DORGRQ  dorgrq_
#    define CUNGRQ  cungrq_
#    define ZUNGRQ  zungrq_

#    define SGELQF  sgelqf_
#    define DGELQF  dgelqf_
#    define CGELQF  cgelqf_
#    define ZGELQF  zgelqf_

#    define SORGLQ  sorglq_
#    define DORGLQ  dorglq_
#    define CUNGLQ  cunglq_
#    define ZUNGLQ  zunglq_

#    define SGEQLF  sgeqlf_
#    define DGEQLF  dgeqlf_
#    define CGEQLF  cgeqlf_
#    define ZGEQLF  zgeqlf_

#    define SORGQL  sorgql_
#    define DORGQL  dorgql_
#    define CUNGQL  cungql_
#    define ZUNGQL  zungql_

#    define SGELS   sgels_
#    define DGELS   dgels_
#    define CGELS   cgels_
#    define ZGELS   zgels_

#    define SGELSY  sgelsy_
#    define DGELSY  dgelsy_
#    define CGELSY  cgelsy_
#    define ZGELSY  zgelsy_

#    define SGELSS  sgelss_
#    define DGELSS  dgelss_
#    define CGELSS  cgelss_
#    define ZGELSS  zgelss_

#    define SGELSD  sgelsd_
#    define DGELSD  dgelsd_
#    define CGELSD  cgelsd_
#    define ZGELSD  zgelsd_

#    define SGGEV   sggev_
#    define DGGEV   dggev_
#    define CGGEV   cggev_
#    define ZGGEV   zggev_

#endif      // !_MSC_VER

void  CVM_FTN_CALL SPOLY       (const tint* m,
                                const float* a,
                                const tint* lda,
                                const tint* n,
                                const float* v,
                                      float* p,
                                const tint* ldp,
                                      float* r);

void  CVM_FTN_CALL DPOLY       (const tint* m,
                                const double* a,
                                const tint* lda,
                                const tint* n,
                                const double* v,
                                      double* p,
                                const tint* ldp,
                                      double* r);

void  CVM_FTN_CALL CPOLY       (const tint* m,
                                const std::complex<float>* a,
                                const tint* lda,
                                const tint* n,
                                const std::complex<float>* v,
                                      std::complex<float>* p,
                                const tint* ldp,
                                      std::complex<float>* r);

void  CVM_FTN_CALL ZPOLY       (const tint* m,
                                const std::complex<double>* a,
                                const tint* lda,
                                const tint* n,
                                const std::complex<double>* v,
                                      std::complex<double>* p,
                                const tint* ldp,
                                      std::complex<double>* r);

tint   CVM_FTN_CALL NPOLY       (const tint* m,
                                const tint* n);

tint   CVM_STD_CALL ISAMAX      (const tint* n,
                                const float* x, 
                                const tint* incx);
tint   CVM_STD_CALL IDAMAX      (const tint* n,
                                const double* x, 
                                const tint* incx);

tint   CVM_STD_CALL ISAMIN      (const tint* n,
                                const float* x,
                                const tint* incx);
tint   CVM_STD_CALL IDAMIN      (const tint* n,
                                const double* x,
                                const tint* incx);

tint   CVM_STD_CALL ICAMAX      (const tint* n,
                                const std::complex<float>* x,
                                const tint* incx);
tint   CVM_STD_CALL IZAMAX      (const tint* n,
                                const std::complex<double>* x,
                                const tint* incx);

tint   CVM_STD_CALL ICAMIN      (const tint* n,
                                const std::complex<float>* x,
                                const tint* incx);
tint   CVM_STD_CALL IZAMIN      (const tint* n,
                                const std::complex<double>* x,
                                const tint* incx);

float  CVM_STD_CALL SNRM2      (const tint* n,
                                const float* x,
                                const tint* incx);
double CVM_STD_CALL DNRM2      (const tint* n,
                                const double* x,
                                const tint* incx);

float  CVM_STD_CALL SCNRM2     (const tint* n,
                                const std::complex<float>* x,
                                const tint* incx);
double CVM_STD_CALL DZNRM2     (const tint* n,
                                const std::complex<double>* x,
                                const tint* incx);

void  CVM_STD_CALL SSWAP       (const tint* n,
                                      float* x,
                                const tint* incx,
                                      float* y,
                                const tint* incy);
void  CVM_STD_CALL DSWAP       (const tint* n,
                                      double* x,
                                const tint* incx,
                                      double* y,
                                const tint* incy);

void  CVM_STD_CALL CSWAP       (const tint* n, 
                                      std::complex<float>* x, 
                                const tint* incx, 
                                      std::complex<float>* y, 
                                const tint* incy);
void  CVM_STD_CALL ZSWAP       (const tint* n, 
                                      std::complex<double>* x, 
                                const tint* incx, 
                                      std::complex<double>* y, 
                                const tint* incy);

float  CVM_STD_CALL SDOT       (const tint* n,
                                const float* x, 
                                const tint* incx, 
                                const float* y, 
                                const tint* incy); 
double CVM_STD_CALL DDOT       (const tint* n,
                                const double* x, 
                                const tint* incx, 
                                const double* y, 
                                const tint* incy); 

// complex dot wrappers
void  CVM_STD_CALL VCDOTU      (std::complex<float>* dot,
                                const tint* n,
                                const std::complex<float>* x, 
                                const tint* incx, 
                                const std::complex<float>* y,
                                const tint* incy);
void  CVM_STD_CALL VZDOTU      (std::complex<double>* dot,
                                const tint* n,
                                const std::complex<double>* x, 
                                const tint* incx, 
                                const std::complex<double>* y,
                                const tint* incy);
void  CVM_STD_CALL VCDOTC      (std::complex<float>* dot,
                                const tint* n,
                                const std::complex<float>* x, 
                                const tint* incx, 
                                const std::complex<float>* y, 
                                const tint* incy);
void  CVM_STD_CALL VZDOTC      (std::complex<double>* dot,
                                const tint* n,
                                const std::complex<double>* x, 
                                const tint* incx, 
                                const std::complex<double>* y, 
                                const tint* incy);

void  CVM_STD_CALL SAXPY       (const tint* n,
                                const float* a, 
                                const float* x, 
                                const tint* incx, 
                                      float* y, 
                                const tint* incy); 
void  CVM_STD_CALL DAXPY       (const tint* n,
                                const double* a, 
                                const double* x, 
                                const tint* incx, 
                                      double* y, 
                                const tint* incy); 

void  CVM_STD_CALL CAXPY       (const tint* n, 
                                const std::complex<float>* a,
                                const std::complex<float>* x, 
                                const tint* incx, 
                                      std::complex<float>* y,
                                const tint* incy); 
void  CVM_STD_CALL ZAXPY       (const tint* n, 
                                const std::complex<double>* a,
                                const std::complex<double>* x, 
                                const tint* incx, 
                                      std::complex<double>* y,
                                const tint* incy); 

void  CVM_STD_CALL DCOPY       (const tint* n,
                                const double* x,
                                const tint* incx, 
                                      double* y, 
                                const tint* incy); 
void  CVM_STD_CALL SCOPY       (const tint* n,
                                const float* x,
                                const tint* incx, 
                                      float* y, 
                                const tint* incy); 

void  CVM_STD_CALL CCOPY       (const tint* n,
                                const std::complex<float>* x, 
                                const tint* incx, 
                                      std::complex<float>* y, 
                                const tint* incy); 
void  CVM_STD_CALL ZCOPY       (const tint* n,
                                const std::complex<double>* x, 
                                const tint* incx, 
                                      std::complex<double>* y, 
                                const tint* incy); 

void  CVM_STD_CALL SSCAL       (const tint* n,
                                const float* a, 
                                      float* x, 
                                const tint* incx);
void  CVM_STD_CALL DSCAL       (const tint* n,
                                const double* a, 
                                      double* x, 
                                const tint* incx);

void  CVM_STD_CALL CSCAL       (const tint* n,
                                const std::complex<float>* a,
                                      std::complex<float>* x, 
                                const tint* incx); 
void  CVM_STD_CALL ZSCAL       (const tint* n,
                                const std::complex<double>* a,
                                      std::complex<double>* x, 
                                const tint* incx); 

void  CVM_STD_CALL CLACGV      (const tint* n,
                                      std::complex<float>* x, 
                                const tint* incx); 
void  CVM_STD_CALL ZLACGV      (const tint* n,
                                      std::complex<double>* x, 
                                const tint* incx); 

void  CVM_STD_CALL CSSCAL      (const tint* n,
                                const float* a, 
                                      std::complex<float>* x, 
                                const tint* incx);
void  CVM_STD_CALL ZDSCAL      (const tint* n,
                                const double* a, 
                                      std::complex<double>* x, 
                                const tint* incx);

void  CVM_STD_CALL SGER        (const tint* m,
                                const tint* n,
                                const float* alpha,
                                const float* x,
                                const tint* incx,
                                const float* y,
                                const tint* incy,
                                      float* a,
                                const tint* lda);
void  CVM_STD_CALL DGER        (const tint* m,
                                const tint* n,
                                const double* alpha,
                                const double* x,
                                const tint* incx,
                                const double* y,
                                const tint* incy,
                                      double* a,
                                const tint* lda);

void  CVM_STD_CALL CGERU       (const tint* m,
                                const tint* n,
                                const std::complex<float>* alpha,
                                const std::complex<float>* x,
                                const tint* incx,
                                const std::complex<float>* y,
                                const tint* incy,
                                      std::complex<float>* a,
                                const tint* lda);
void  CVM_STD_CALL ZGERU       (const tint* m,
                                const tint* n,
                                const std::complex<double>* alpha,
                                const std::complex<double>* x,
                                const tint* incx,
                                const std::complex<double>* y,
                                const tint* incy,
                                      std::complex<double>* a,
                                const tint* lda);

void  CVM_STD_CALL CGERC       (const tint* m,
                                const tint* n,
                                const std::complex<float>* alpha,
                                const std::complex<float>* x,
                                const tint* incx,
                                const std::complex<float>* y,
                                const tint* incy,
                                      std::complex<float>* a,
                                const tint* lda);
void  CVM_STD_CALL ZGERC       (const tint* m,
                                const tint* n,
                                const std::complex<double>* alpha,
                                const std::complex<double>* x,
                                const tint* incx,
                                const std::complex<double>* y,
                                const tint* incy,
                                      std::complex<double>* a,
                                const tint* lda);

void  CVM_STD_CALL SGEMV       (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* m,
                                const tint* n,
                                const float* alpha,
                                const float* a,
                                const tint* lda,
                                const float* x,
                                const tint* incx,
                                const float* beta,
                                      float* y,
                                const tint* incy);
void  CVM_STD_CALL DGEMV       (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* m,
                                const tint* n,
                                const double* alpha,
                                const double* a,
                                const tint* lda,
                                const double* x,
                                const tint* incx,
                                const double* beta,
                                      double* y,
                                const tint* incy);

void  CVM_STD_CALL CGEMV       (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* m,
                                const tint* n,
                                const std::complex<float>* alpha,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* x,
                                const tint* incx,
                                const std::complex<float>* beta,
                                      std::complex<float>* y,
                                const tint* incy);
void  CVM_STD_CALL ZGEMV       (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* m,
                                const tint* n,
                                const std::complex<double>* alpha,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* x,
                                const tint* incx,
                                const std::complex<double>* beta,
                                      std::complex<double>* y,
                                const tint* incy);

void  CVM_STD_CALL SGBMV       (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                const float* alpha,
                                const float* a,
                                const tint* lda,
                                const float* x,
                                const tint* incx,
                                const float* beta,
                                      float* y,
                                const tint* incy);
void  CVM_STD_CALL DGBMV       (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                const double* alpha,
                                const double* a,
                                const tint* lda,
                                const double* x,
                                const tint* incx,
                                const double* beta,
                                      double* y,
                                const tint* incy);
void  CVM_STD_CALL CGBMV       (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                const std::complex<float>* alpha,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* x,
                                const tint* incx,
                                const std::complex<float>* beta,
                                      std::complex<float>* y,
                                const tint* incy);
void  CVM_STD_CALL ZGBMV       (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                const std::complex<double>* alpha,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* x,
                                const tint* incx,
                                const std::complex<double>* beta,
                                      std::complex<double>* y,
                                const tint* incy);

void  CVM_STD_CALL DGEMM       (const char* transa,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transasz,
#endif
                                const char* transb,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transbsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* k,
                                const double* alpha,
                                const double* a,
                                const tint* lda,
                                const double* b,
                                const tint* ldb,
                                const double* beta,
                                      double* c,
                                const tint* ldc);

void  CVM_STD_CALL SGEMM       (const char* transa,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transasz,
#endif
                                const char* transb,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transbsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* k,
                                const float* alpha,
                                const float* a,
                                const tint* lda,
                                const float* b,
                                const tint* ldb,
                                const float* beta,
                                      float* c,
                                const tint* ldc);

void  CVM_STD_CALL CGEMM       (const char* transa,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transasz,
#endif
                                const char* transb,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transbsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* k,
                                const std::complex<float>* alpha,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* b,
                                const tint* ldb,
                                const std::complex<float>* beta,
                                      std::complex<float>* c,
                                const tint* ldc);
void  CVM_STD_CALL ZGEMM       (const char* transa,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transasz,
#endif
                                const char* transb,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transbsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* k,
                                const std::complex<double>* alpha,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* b,
                                const tint* ldb,
                                const std::complex<double>* beta,
                                      std::complex<double>* c,
                                const tint* ldc);

void  CVM_FTN_CALL SMEXP       (const tint* m,
                                const float* a,
                                const tint* lda,
                                      float* ea,
                                const tint* lde,
                                      float* r,
                                      tint* ir,
                                      tint* nr,
                                      tint* ni,
                                      tint* nq,
                                      tint* j,
                                const tint* issymm,                  // LOGICAL is 4-byte tint
                                      float* work,
                                const tint* lwork);
                                      
void  CVM_FTN_CALL DMEXP       (const tint* m,
                                const double* a,
                                const tint* lda,
                                      double* ea,
                                const tint* lde,
                                      double* r,
                                      tint* ir,
                                      tint* nr,
                                      tint* ni,
                                      tint* nq,
                                      tint* j,
                                const tint* issymm,                  // LOGICAL is 4-byte tint
                                      double* work,
                                const tint* lwork);

void  CVM_FTN_CALL CMEXP       (const tint* m,
                                const std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* ea,
                                const tint* lde,
                                      std::complex<float>* r,
                                      tint* ir,
                                      tint* nr,
                                      tint* ni,
                                      tint* nq,
                                      tint* j,
                                const tint* issymm,                  // LOGICAL is 4-byte tint
                                      std::complex<float>* work,
                                const tint* lwork);

void  CVM_FTN_CALL ZMEXP       (const tint* m,
                                const std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* ea,
                                const tint* lde,
                                      std::complex<double>* r,
                                      tint* ir,
                                      tint* nr,
                                      tint* ni,
                                      tint* nq,
                                      tint* j,
                                const tint* issymm,                  // LOGICAL is 4-byte tint
                                      std::complex<double>* work,
                                const tint* lwork);

void  CVM_FTN_CALL SMEXPC      (const tint* m,
                                const float* a,
                                const tint* lda,
                                const float* tol,
                                      tint* nr,
                                      tint* ni,
                                      tint* nq,
                                      tint* j);

void  CVM_FTN_CALL DMEXPC      (const tint* m,
                                const double* a,
                                const tint* lda,
                                const double* tol,
                                      tint* nr,
                                      tint* ni,
                                      tint* nq,
                                      tint* j);

void  CVM_FTN_CALL CMEXPC      (const tint* m,
                                const std::complex<float>* a,
                                const tint* lda,
                                const float* tol,
                                      tint* nr,
                                      tint* ni,
                                      tint* nq,
                                      tint* j);

void  CVM_FTN_CALL ZMEXPC      (const tint* m,
                                const std::complex<double>* a,
                                const tint* lda,
                                const double* tol,
                                      tint* nr,
                                      tint* ni,
                                      tint* nq,
                                      tint* j);

void  CVM_STD_CALL SGETRF      (const tint* m,
                                const tint* n, 
                                      float* a,
                                const tint* lda,
                                      tint* ipiv, 
                                      tint* info);
void  CVM_STD_CALL DGETRF      (const tint* m,
                                const tint* n,
                                      double* a,
                                const tint* lda,
                                      tint* ipiv,
                                      tint* info);
void  CVM_STD_CALL CGETRF      (const tint* m,
                                const tint* n, 
                                      std::complex<float>* a,
                                const tint* lda,
                                      tint* ipiv, 
                                      tint* info);
void  CVM_STD_CALL ZGETRF      (const tint* m,
                                const tint* n, 
                                      std::complex<double>* a,
                                const tint* lda,
                                      tint* ipiv, 
                                      tint* info);

void  CVM_STD_CALL SGBTRF      (const tint* m,
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                      float* a,
                                const tint* lda,
                                      tint* ipiv,
                                      tint* info);
void  CVM_STD_CALL DGBTRF      (const tint* m,
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                      double* a,
                                const tint* lda,
                                      tint* ipiv,
                                      tint* info);
void  CVM_STD_CALL CGBTRF      (const tint* m,
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                      std::complex<float>* a,
                                const tint* lda,
                                      tint* ipiv,
                                      tint* info);
void  CVM_STD_CALL ZGBTRF      (const tint* m,
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                      std::complex<double>* a,
                                const tint* lda,
                                      tint* ipiv,
                                      tint* info);

void  CVM_STD_CALL SGETRS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* nrhs, 
                                const float* a,
                                const tint* lda,
                                      tint* ipiv, 
                                      float* b,
                                const tint* ldb,
                                      tint* info);
void  CVM_STD_CALL DGETRS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* nrhs, 
                                const double* a,
                                const tint* lda,
                                      tint* ipiv, 
                                      double* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL SGBTRS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                const tint* nrhs,
                                const float* a,
                                const tint* lda,
                                      tint* ipiv,
                                      float* b,
                                const tint* ldb,
                                      tint* info);
void  CVM_STD_CALL DGBTRS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                const tint* nrhs,
                                const double* a,
                                const tint* lda,
                                      tint* ipiv,
                                      double* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL SGERFS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* nrhs, 
                                const float* a,
                                const tint* lda,
                                const float* af,
                                const tint* ldaf,
                                      tint* ipiv, 
                                const float* b,
                                const tint* ldb,
                                      float* x,
                                const tint* ldx,
                                      float* ferr,
                                      float* berr,
                                      float* work,
                                      tint* iwork,
                                      tint* info);
void  CVM_STD_CALL DGERFS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* nrhs, 
                                const double* a,
                                const tint* lda,
                                const double* af,
                                const tint* ldaf,
                                      tint* ipiv, 
                                const double* b,
                                const tint* ldb,
                                      double* x,
                                const tint* ldx,
                                      double* ferr,
                                      double* berr,
                                      double* work,
                                      tint* iwork,
                                      tint* info);

void  CVM_STD_CALL SGBRFS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                const tint* nrhs,
                                const float* a,
                                const tint* lda,
                                const float* af,
                                const tint* ldaf,
                                      tint* ipiv,
                                const float* b,
                                const tint* ldb,
                                      float* x,
                                const tint* ldx,
                                      float* ferr,
                                      float* berr,
                                      float* work,
                                      tint* iwork,
                                      tint* info);
void  CVM_STD_CALL DGBRFS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                const tint* nrhs,
                                const double* a,
                                const tint* lda,
                                const double* af,
                                const tint* ldaf,
                                      tint* ipiv,
                                const double* b,
                                const tint* ldb,
                                      double* x,
                                const tint* ldx,
                                      double* ferr,
                                      double* berr,
                                      double* work,
                                      tint* iwork,
                                      tint* info);

void  CVM_STD_CALL CGETRS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* nrhs, 
                                const std::complex<float>* a,
                                const tint* lda,
                                      tint* ipiv, 
                                      std::complex<float>* b,
                                const tint* ldb,
                                      tint* info);
void  CVM_STD_CALL ZGETRS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* nrhs, 
                                const std::complex<double>* a,
                                const tint* lda,
                                      tint* ipiv, 
                                      std::complex<double>* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL CGBTRS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                const tint* nrhs,
                                const std::complex<float>* a,
                                const tint* lda,
                                      tint* ipiv,
                                      std::complex<float>* b,
                                const tint* ldb,
                                      tint* info);
void  CVM_STD_CALL ZGBTRS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                const tint* nrhs,
                                const std::complex<double>* a,
                                const tint* lda,
                                      tint* ipiv,
                                      std::complex<double>* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL CGERFS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* nrhs, 
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* af,
                                const tint* ldaf,
                                      tint* ipiv, 
                                const std::complex<float>* b,
                                const tint* ldb,
                                      std::complex<float>* x,
                                const tint* ldx,
                                      float* ferr,
                                      float* berr,
                                      std::complex<float>* work,
                                      float* rwork,
                                      tint* info);
void  CVM_STD_CALL ZGERFS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* nrhs, 
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* af,
                                const tint* ldaf,
                                      tint* ipiv, 
                                const std::complex<double>* b,
                                const tint* ldb,
                                      std::complex<double>* x,
                                const tint* ldx,
                                      double* ferr,
                                      double* berr,
                                      std::complex<double>* work,
                                      double* rwork,
                                      tint* info);

void  CVM_STD_CALL CGBRFS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                const tint* nrhs,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* af,
                                const tint* ldaf,
                                      tint* ipiv,
                                const std::complex<float>* b,
                                const tint* ldb,
                                      std::complex<float>* x,
                                const tint* ldx,
                                      float* ferr,
                                      float* berr,
                                      std::complex<float>* work,
                                      float* rwork,
                                      tint* info);
void  CVM_STD_CALL ZGBRFS      (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* kl,
                                const tint* ku,
                                const tint* nrhs,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* af,
                                const tint* ldaf,
                                      tint* ipiv,
                                const std::complex<double>* b,
                                const tint* ldb,
                                      std::complex<double>* x,
                                const tint* ldx,
                                      double* ferr,
                                      double* berr,
                                      std::complex<double>* work,
                                      double* rwork,
                                      tint* info);

void  CVM_STD_CALL SGETRI      (const tint* n, 
                                      float* a,
                                const tint* lda,
                                      tint* ipiv, 
                                      float* work,
                                const tint* lwork,
                                      tint* info);
void  CVM_STD_CALL DGETRI      (const tint* n, 
                                      double* a,
                                const tint* lda,
                                      tint* ipiv, 
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL CGETRI      (const tint* n, 
                                      std::complex<float>* a,
                                const tint* lda,
                                      tint* ipiv, 
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);
void  CVM_STD_CALL ZGETRI      (const tint* n, 
                                      std::complex<double>* a,
                                const tint* lda,
                                      tint* ipiv, 
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL SGEBRD      (const tint* m,
                                const tint* n,
                                      float* a,
                                const tint* lda,
                                      float* d,
                                      float* e,
                                      float* tauq,
                                      float* taup,
                                      float* work,
                                const tint* lwork,
                                      tint* info);
void  CVM_STD_CALL DGEBRD      (const tint* m,
                                const tint* n,
                                      double* a,
                                const tint* lda,
                                      double* d,
                                      double* e,
                                      double* tauq,
                                      double* taup,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL CGEBRD      (const tint* m,
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                      float* d,
                                      float* e,
                                      std::complex<float>* tauq,
                                      std::complex<float>* taup,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);
void  CVM_STD_CALL ZGEBRD      (const tint* m,
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                      double* d,
                                      double* e,
                                      std::complex<double>* tauq,
                                      std::complex<double>* taup,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL SGBBRD      (const char* vect,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint vectsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* ncc,
                                const tint* kl,
                                const tint* ku,
                                      float* ab,
                                const tint* ldab,
                                      float* d,
                                      float* e,
                                      float* q,
                                const tint* ldq,
                                      float* pt,
                                const tint* ldpt,
                                      float* c,
                                const tint* ldc,
                                      float* work,
                                      tint* info);

void  CVM_STD_CALL DGBBRD      (const char* vect,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint vectsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* ncc,
                                const tint* kl,
                                const tint* ku,
                                      double* ab,
                                const tint* ldab,
                                      double* d,
                                      double* e,
                                      double* q,
                                const tint* ldq,
                                      double* pt,
                                const tint* ldpt,
                                      double* c,
                                const tint* ldc,
                                      double* work,
                                      tint* info);

void  CVM_STD_CALL CGBBRD      (const char* vect,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint vectsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* ncc,
                                const tint* kl,
                                const tint* ku,
                                      std::complex<float>* ab,
                                const tint* ldab,
                                      float* d,
                                      float* e,
                                      std::complex<float>* q,
                                const tint* ldq,
                                      std::complex<float>* pt,
                                const tint* ldpt,
                                      std::complex<float>* c,
                                const tint* ldc,
                                      std::complex<float>* work,
                                      float* rwork,
                                      tint* info);

void  CVM_STD_CALL ZGBBRD      (const char* vect,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint vectsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* ncc,
                                const tint* kl,
                                const tint* ku,
                                      std::complex<double>* ab,
                                const tint* ldab,
                                      double* d,
                                      double* e,
                                      std::complex<double>* q,
                                const tint* ldq,
                                      std::complex<double>* pt,
                                const tint* ldpt,
                                      std::complex<double>* c,
                                const tint* ldc,
                                      std::complex<double>* work,
                                      double* rwork,
                                      tint* info);

void  CVM_STD_CALL SORGBR      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* k,
                                      float* a,
                                const tint* lda,
                                      float* tau,
                                      float* work,
                                const tint* lwork,
                                      tint* info);
void  CVM_STD_CALL DORGBR      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* k,
                                      double* a,
                                const tint* lda,
                                      double* tau,
                                      double* work,
                                const tint* lwork,
                                      tint* info
                                      );

void  CVM_STD_CALL CUNGBR      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* k,
                                      std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* tau,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);
void  CVM_STD_CALL ZUNGBR      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* k,
                                      std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* tau,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL SBDSQR      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* ncvt,
                                const tint* nru,
                                const tint* ncc,
                                      float* d,
                                      float* e,
                                      float* vt,
                                const tint* ldvt,
                                      float* u,
                                const tint* ldu,
                                      float* c,
                                const tint* ldc,
                                      float* work,
                                      tint* info);
void  CVM_STD_CALL DBDSQR      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* ncvt,
                                const tint* nru,
                                const tint* ncc,
                                      double* d,
                                      double* e,
                                      double* vt,
                                const tint* ldvt,
                                      double* u,
                                const tint* ldu,
                                      double* c,
                                const tint* ldc,
                                      double* work,
                                      tint* info);

void  CVM_STD_CALL CBDSQR      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* ncvt,
                                const tint* nru,
                                const tint* ncc,
                                      float* d,
                                      float* e,
                                      std::complex<float>* vt,
                                const tint* ldvt,
                                      std::complex<float>* u,
                                const tint* ldu,
                                      std::complex<float>* c,
                                const tint* ldc,
                                      float* work,
                                      tint* info);
void  CVM_STD_CALL ZBDSQR      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* ncvt,
                                const tint* nru,
                                const tint* ncc,
                                      double* d,
                                      double* e,
                                      std::complex<double>* vt,
                                const tint* ldvt,
                                      std::complex<double>* u,
                                const tint* ldu,
                                      std::complex<double>* c,
                                const tint* ldc,
                                      double* work,
                                      tint* info);

void  CVM_STD_CALL SGEBAL      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const tint* n,
                                      float* a,
                                const tint* lda,

                                      tint* ilo,
                                      tint* ihi,
                                      float* scale,
                                      tint* info);
void  CVM_STD_CALL DGEBAL      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const tint* n,
                                      double* a,
                                const tint* lda,

                                      tint* ilo,
                                      tint* ihi,
                                      double* scale,
                                      tint* info);

void  CVM_STD_CALL CGEBAL      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                      tint* ilo,
                                      tint* ihi,
                                      float* scale,
                                      tint* info);
void  CVM_STD_CALL ZGEBAL      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                      tint* ilo,
                                      tint* ihi,
                                      double* scale,
                                      tint* info);

void  CVM_STD_CALL SGEHRD      (const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      float* a,
                                const tint* lda,
                                      float* tau,
                                      float* work,
                                const tint* lwork,
                                      tint* info);
void  CVM_STD_CALL DGEHRD      (const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      double* a,
                                const tint* lda,
                                      double* tau,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL CGEHRD      (const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* tau,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);
void  CVM_STD_CALL ZGEHRD      (const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* tau,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL SORGHR      (const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      float* a,
                                const tint* lda,
                                      float* tau,
                                      float* work,
                                const tint* lwork,
                                      tint* info);
void  CVM_STD_CALL DORGHR      (const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      double* a,
                                const tint* lda,
                                      double* tau,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL CUNGHR      (const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* tau,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);
void  CVM_STD_CALL ZUNGHR      (const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* tau,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL SHSEQR      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* compz,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint compzsz,
#endif
                                const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      float* h,
                                const tint* ldh,
                                      float* wr,
                                      float* wi,
                                      float* z,
                                const tint* ldz,
                                      float* work,
                                const tint* lwork,
                                      tint* info);
void  CVM_STD_CALL DHSEQR      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* compz,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint compzsz,
#endif
                                const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      double* h,
                                const tint* ldh,
                                      double* wr,
                                      double* wi,
                                      double* z,
                                const tint* ldz,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL CHSEQR      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* compz,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint compzsz,
#endif
                                const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      std::complex<float>* h,
                                const tint* ldh,
                                      std::complex<float>* w,
                                      std::complex<float>* z,
                                const tint* ldz,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);
void  CVM_STD_CALL ZHSEQR      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* compz,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint compzsz,
#endif
                                const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      std::complex<double>* h,
                                const tint* ldh,
                                      std::complex<double>* w,
                                      std::complex<double>* z,
                                const tint* ldz,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL STREVC      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* howmny,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint howmnysz,
#endif
                                const tint* select,                                  // CAUTION! We assume that LOGICAL datatype is 4-byte long
                                const tint* n,
                                      float* t,
                                const tint* ldt,
                                      float* vl,
                                const tint* ldvl,
                                      float* vr,
                                const tint* ldvr,
                                const tint* mm,
                                      tint* m,
                                      float* work,
                                      tint* info);
void  CVM_STD_CALL DTREVC      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* howmny,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint howmnysz,
#endif
                                const tint* select,                                  // CAUTION! We assume that LOGICAL datatype is 4-byte long
                                const tint* n,
                                      double* t,
                                const tint* ldt,
                                      double* vl,
                                const tint* ldvl,
                                      double* vr,
                                const tint* ldvr,
                                const tint* mm,
                                      tint* m,
                                      double* work,
                                      tint* info);

void  CVM_STD_CALL CTREVC      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* howmny,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint howmnysz,
#endif
                                const tint* select,                                  // CAUTION! We assume that LOGICAL datatype is 4-byte long
                                const tint* n,
                                      std::complex<float>* t,
                                const tint* ldt,
                                      std::complex<float>* vl,
                                const tint* ldvl,
                                      std::complex<float>* vr,
                                const tint* ldvr,
                                const tint* mm,
                                      tint* m,
                                      std::complex<float>* work,
                                      float* rwork,
                                      tint* info);
void  CVM_STD_CALL ZTREVC      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* howmny,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint howmnysz,
#endif
                                const tint* select,                                  // CAUTION! We assume that LOGICAL datatype is 4-byte long
                                const tint* n,
                                      std::complex<double>* t,
                                const tint* ldt,
                                      std::complex<double>* vl,
                                const tint* ldvl,
                                      std::complex<double>* vr,
                                const tint* ldvr,
                                const tint* mm,
                                      tint* m,
                                      std::complex<double>* work,
                                      double* rwork,
                                      tint* info);

void  CVM_STD_CALL SGEBAK      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* side,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint sidesz,
#endif
                                const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      float* scale,
                                const tint* m,
                                      float* v,
                                const tint* ldv,
                                      tint* info);
void  CVM_STD_CALL DGEBAK      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* side,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint sidesz,
#endif
                                const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      double* scale,
                                const tint* m,
                                      double* v,
                                const tint* ldv,
                                      tint* info);

void  CVM_STD_CALL CGEBAK      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* side,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint sidesz,
#endif
                                const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      float* scale,
                                const tint* m,
                                      std::complex<float>* v,
                                const tint* ldv,
                                      tint* info);
void  CVM_STD_CALL ZGEBAK      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* side,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint sidesz,
#endif
                                const tint* n,
                                const tint* ilo,
                                const tint* ihi,
                                      double* scale,
                                const tint* m,
                                      std::complex<double>* v,
                                const tint* ldv,
                                      tint* info);

void  CVM_STD_CALL SGECON      (const char* norm,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint normsz,
#endif
                                const tint* n,
                                      float* a,                                     // const
                                const tint* lda,
                                const float* anorm,
                                      float* rcond,
                                      float* work,
                                      tint* iwork,
                                      tint* info);
void  CVM_STD_CALL DGECON      (const char* norm,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint normsz,
#endif
                                const tint* n,
                                      double* a,                                    // const
                                const tint* lda,
                                const double* anorm,
                                      double* rcond,
                                      double* work,
                                      tint* iwork,
                                      tint* info);

void  CVM_STD_CALL CGECON      (const char* norm,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint normsz,
#endif
                                const tint* n,
                                      std::complex<float>* a,                       // const
                                const tint* lda,
                                const float* anorm,
                                      float* rcond,
                                      std::complex<float>* work,
                                      float* rwork,
                                      tint* info);
void  CVM_STD_CALL ZGECON      (const char* norm,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint normsz,
#endif
                                const tint* n,
                                      std::complex<double>* a,                      // const
                                const tint* lda,
                                const double* anorm,
                                      double* rcond,
                                      std::complex<double>* work,
                                      double* rwork,
                                      tint* info);

void  CVM_STD_CALL SSPMV       (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const float* alpha,
                                const float* ap,
                                const float* x,
                                const tint* incx,
                                const float* beta,
                                      float* y,
                                const tint* incy);

void  CVM_STD_CALL DSPMV       (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const double* alpha,
                                const double* ap,
                                const double* x,
                                const tint* incx,
                                const double* beta,
                                      double* y,
                                const tint* incy);

void  CVM_STD_CALL SSYMM       (const char* side,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint sidesz,
#endif
                                const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* m,
                                const tint* n,
                                const float* alpha,
                                const float* a,
                                const tint* lda,
                                const float* b,
                                const tint* ldb,
                                const float* beta,
                                      float* c,
                                const tint* ldc);

void  CVM_STD_CALL DSYMM       (const char* side,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint sidesz,
#endif
                                const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* m,
                                const tint* n,
                                const double* alpha,
                                const double* a,
                                const tint* lda,
                                const double* b,
                                const tint* ldb,
                                const double* beta,
                                      double* c,
                                const tint* ldc);

void  CVM_STD_CALL CSYMM       (const char* side,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint sidesz,
#endif
                                const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* m,
                                const tint* n,
                                const std::complex<float>* alpha,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* b,
                                const tint* ldb,
                                const std::complex<float>* beta,
                                      std::complex<float>* c,
                                const tint* ldc);

void  CVM_STD_CALL ZSYMM       (const char* side,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint sidesz,
#endif
                                const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* m,
                                const tint* n,
                                const std::complex<double>* alpha,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* b,
                                const tint* ldb,
                                const std::complex<double>* beta,
                                      std::complex<double>* c,
                                const tint* ldc);

void  CVM_STD_CALL CHEMM       (const char* side,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint sidesz,
#endif
                                const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* m,
                                const tint* n,
                                const std::complex<float>* alpha,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* b,
                                const tint* ldb,
                                const std::complex<float>* beta,
                                      std::complex<float>* c,
                                const tint* ldc);

void  CVM_STD_CALL ZHEMM       (const char* side,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint sidesz,
#endif
                                const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* m,
                                const tint* n,
                                const std::complex<double>* alpha,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* b,
                                const tint* ldb,
                                const std::complex<double>* beta,
                                      std::complex<double>* c,
                                const tint* ldc);

void  CVM_STD_CALL SPOTRF      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      float* a,
                                const tint* lda,
                                      tint* info);

void  CVM_STD_CALL DPOTRF      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      double* a,
                                const tint* lda,
                                      tint* info);

void  CVM_STD_CALL CPOTRF      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                      tint* info);

void  CVM_STD_CALL ZPOTRF      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                      tint* info);

void  CVM_STD_CALL SSYTRF      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      float* a,
                                const tint* lda,
                                      tint* ipiv,
                                      float* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL DSYTRF      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      double* a,
                                const tint* lda,
                                      tint* ipiv,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL CSYTRF      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                      tint* ipiv,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL ZSYTRF      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                      tint* ipiv,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL CHETRF      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                      tint* ipiv,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL ZHETRF      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                      tint* ipiv,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

void  CVM_STD_CALL SPOTRS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const float* a,
                                const tint* lda,
                                      float* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL DPOTRS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const double* a,
                                const tint* lda,
                                      double* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL CPOTRS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL ZPOTRS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL SPORFS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const float* a,
                                const tint* lda,
                                const float* af,
                                const tint* ldaf,
                                const float* b,
                                const tint* ldb,
                                      float* x,
                                const tint* ldx,
                                      float* ferr,
                                      float* berr,
                                      float* work,
                                      tint* iwork,
                                      tint* info);

void  CVM_STD_CALL DPORFS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const double* a,
                                const tint* lda,
                                const double* af,
                                const tint* ldaf,
                                const double* b,
                                const tint* ldb,
                                      double* x,
                                const tint* ldx,
                                      double* ferr,
                                      double* berr,
                                      double* work,
                                      tint* iwork,
                                      tint* info);

void  CVM_STD_CALL CPORFS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* af,
                                const tint* ldaf,
                                const std::complex<float>* b,
                                const tint* ldb,
                                      std::complex<float>* x,
                                const tint* ldx,
                                      float* ferr,
                                      float* berr,
                                      std::complex<float>* work,
                                      float* rwork,
                                      tint* info);

void  CVM_STD_CALL ZPORFS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* af,
                                const tint* ldaf,
                                const std::complex<double>* b,
                                const tint* ldb,
                                      std::complex<double>* x,
                                const tint* ldx,
                                      double* ferr,
                                      double* berr,
                                      std::complex<double>* work,
                                      double* rwork,
                                      tint* info);

void  CVM_STD_CALL SSYTRS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const float* a,
                                const tint* lda,
                                const tint* ipiv,
                                      float* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL DSYTRS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const double* a,
                                const tint* lda,
                                const tint* ipiv,
                                      double* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL CSYTRS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const std::complex<float>* a,
                                const tint* lda,
                                const tint* ipiv,
                                      std::complex<float>* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL ZSYTRS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const std::complex<double>* a,
                                const tint* lda,
                                const tint* ipiv,
                                      std::complex<double>* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL CHETRS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const std::complex<float>* a,
                                const tint* lda,
                                const tint* ipiv,
                                      std::complex<float>* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL ZHETRS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const std::complex<double>* a,
                                const tint* lda,
                                const tint* ipiv,
                                      std::complex<double>* b,
                                const tint* ldb,
                                      tint* info);

void  CVM_STD_CALL SSYRFS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const float* a,
                                const tint* lda,
                                const float* af,
                                const tint* ldaf,
                                const tint* ipiv,
                                const float* b,
                                const tint* ldb,
                                      float* x,
                                const tint* ldx,
                                      float* ferr,
                                      float* berr,
                                      float* work,
                                      tint* iwork,
                                      tint* info);

void  CVM_STD_CALL DSYRFS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const double* a,
                                const tint* lda,
                                const double* af,
                                const tint* ldaf,
                                const tint* ipiv,
                                const double* b,
                                const tint* ldb,
                                      double* x,
                                const tint* ldx,
                                      double* ferr,
                                      double* berr,
                                      double* work,
                                      tint* iwork,
                                      tint* info);

void  CVM_STD_CALL CSYRFS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* af,
                                const tint* ldaf,
                                const tint* ipiv,
                                const std::complex<float>* b,
                                const tint* ldb,
                                      std::complex<float>* x,
                                const tint* ldx,
                                      float* ferr,
                                      float* berr,
                                      std::complex<float>* work,
                                      float* rwork,
                                      tint* info);

void  CVM_STD_CALL ZSYRFS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* af,
                                const tint* ldaf,
                                const tint* ipiv,
                                const std::complex<double>* b,
                                const tint* ldb,
                                      std::complex<double>* x,
                                const tint* ldx,
                                      double* ferr,
                                      double* berr,
                                      std::complex<double>* work,
                                      double* rwork,
                                      tint* info);

void  CVM_STD_CALL CHERFS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* af,
                                const tint* ldaf,
                                const tint* ipiv,
                                const std::complex<float>* b,
                                const tint* ldb,
                                      std::complex<float>* x,
                                const tint* ldx,
                                      float* ferr,
                                      float* berr,
                                      std::complex<float>* work,
                                      float* rwork,
                                      tint* info);

void  CVM_STD_CALL ZHERFS      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const tint* nrhs,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* af,
                                const tint* ldaf,
                                const tint* ipiv,
                                const std::complex<double>* b,
                                const tint* ldb,
                                      std::complex<double>* x,
                                const tint* ldx,
                                      double* ferr,
                                      double* berr,
                                      std::complex<double>* work,
                                      double* rwork,
                                      tint* info);

void  CVM_STD_CALL SPOTRI      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      float* a,
                                const tint* lda,
                                      tint* info);

void  CVM_STD_CALL DPOTRI      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      double* a,
                                const tint* lda,
                                      tint* info);

void  CVM_STD_CALL CPOTRI      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                      tint* info);

void  CVM_STD_CALL ZPOTRI      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                      tint* info);

void  CVM_STD_CALL SSYTRI      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      float* a,
                                const tint* lda,
                                const tint* ipiv,
                                      float* work,
                                      tint* info);

void  CVM_STD_CALL DSYTRI      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      double* a,
                                const tint* lda,
                                const tint* ipiv,
                                      double* work,
                                      tint* info);

void  CVM_STD_CALL CSYTRI      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                const tint* ipiv,
                                      std::complex<float>* work,
                                      tint* info);

void  CVM_STD_CALL ZSYTRI      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                const tint* ipiv,
                                      std::complex<double>* work,
                                      tint* info);

void  CVM_STD_CALL CHETRI      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                const tint* ipiv,
                                      std::complex<float>* work,
                                      tint* info);

void  CVM_STD_CALL ZHETRI      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                const tint* ipiv,
                                      std::complex<double>* work,
                                      tint* info);

void  CVM_STD_CALL SSYEVD      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      float* a,
                                const tint* lda,
                                      float* w,
                                      float* work,
                                      tint* lwork,
                                      tint* iwork,
                                      tint* liwork,
                                      tint* info);

void  CVM_STD_CALL DSYEVD      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      double* a,
                                const tint* lda,
                                      double* w,
                                      double* work,
                                      tint* lwork,
                                      tint* iwork,
                                      tint* liwork,
                                      tint* info);

void  CVM_STD_CALL CHEEVD      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                      float* w,
                                      std::complex<float>* work,
                                      tint* lwork,
                                      float* rwork,
                                      tint* lrwork,
                                      tint* iwork,
                                      tint* liwork,
                                      tint* info);

void  CVM_STD_CALL ZHEEVD      (const char* job,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobsz,
#endif
                                const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                      double* w,
                                      std::complex<double>* work,
                                      tint* lwork,
                                      double* rwork,
                                      tint* lrwork,
                                      tint* iwork,
                                      tint* liwork,
                                      tint* info);

void  CVM_STD_CALL SPOEQU      (const tint* n,
                                const float* a,
                                const tint* lda,
                                      float* s,
                                      float* scond,
                                      float* amax,
                                      tint* info);

void  CVM_STD_CALL DPOEQU      (const tint* n,
                                const double* a,
                                const tint* lda,
                                      double* s,
                                      double* scond,
                                      double* amax,
                                      tint* info);

void  CVM_STD_CALL CPOEQU      (const tint* n,
                                const std::complex<float>* a,
                                const tint* lda,
                                      float* s,
                                      float* scond,
                                      float* amax,
                                      tint* info);

void  CVM_STD_CALL ZPOEQU      (const tint* n,
                                const std::complex<double>* a,
                                const tint* lda,
                                      double* s,
                                      double* scond,
                                      double* amax,
                                      tint* info);

void  CVM_STD_CALL SSYMV       (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const float* alpha,
                                const float* a,
                                const tint* lda,
                                const float* x,
                                const tint* incx,
                                const float* beta,
                                      float* y,
                                const tint* incy);

void  CVM_STD_CALL DSYMV       (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const double* alpha,
                                const double* a,
                                const tint* lda,
                                const double* x,
                                const tint* incx,
                                const double* beta,
                                      double* y,
                                const tint* incy);

void  CVM_STD_CALL CHEMV       (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const std::complex<float>* alpha,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* x,
                                const tint* incx,
                                const std::complex<float>* beta,
                                      std::complex<float>* y,
                                const tint* incy);

void  CVM_STD_CALL ZHEMV       (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const tint* n,
                                const std::complex<double>* alpha,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* x,
                                const tint* incx,
                                const std::complex<double>* beta,
                                      std::complex<double>* y,
                                const tint* incy);


void  CVM_STD_CALL SSYRK       (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* k,
                                const float* alpha,
                                const float* a,
                                const tint* lda,
                                const float* beta,
                                      float* c,
                                const tint* ldc);

void  CVM_STD_CALL DSYRK       (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* k,
                                const double* alpha,
                                const double* a,
                                const tint* lda,
                                const double* beta,
                                      double* c,
                                const tint* ldc);

void  CVM_STD_CALL CSYRK       (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* k,
                                const std::complex<float>* alpha,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* beta,
                                      std::complex<float>* c,
                                const tint* ldc);

void  CVM_STD_CALL ZSYRK       (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* k,
                                const std::complex<double>* alpha,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* beta,
                                      std::complex<double>* c,
                                const tint* ldc);

void  CVM_STD_CALL CHERK       (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* k,
                                const float* alpha,
                                const std::complex<float>* a,
                                const tint* lda,
                                const float* beta,
                                      std::complex<float>* c,
                                const tint* ldc);

void  CVM_STD_CALL ZHERK       (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* k,
                                const double* alpha,
                                const std::complex<double>* a,
                                const tint* lda,
                                const double* beta,
                                      std::complex<double>* c,
                                const tint* ldc);

void  CVM_STD_CALL SSYR2K      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* k,
                                const float* alpha,
                                const float* a,
                                const tint* lda,
                                const float* b,
                                const tint* ldb,
                                const float* beta,
                                      float* c,
                                const tint* ldc);

void  CVM_STD_CALL DSYR2K      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* k,
                                const double* alpha,
                                const double* a,
                                const tint* lda,
                                const double* b,
                                const tint* ldb,
                                const double* beta,
                                      double* c,
                                const tint* ldc);

void  CVM_STD_CALL CSYR2K      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* k,
                                const std::complex<float>* alpha,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* b,
                                const tint* ldb,
                                const std::complex<float>* beta,
                                      std::complex<float>* c,
                                const tint* ldc);

void  CVM_STD_CALL ZSYR2K      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* k,
                                const std::complex<double>* alpha,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* b,
                                const tint* ldb,
                                const std::complex<double>* beta,
                                      std::complex<double>* c,
                                const tint* ldc);

void  CVM_STD_CALL CHER2K      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* k,
                                const std::complex<float>* alpha,
                                const std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* b,
                                const tint* ldb,
                                const float* beta,
                                      std::complex<float>* c,
                                const tint* ldc);

void  CVM_STD_CALL ZHER2K      (const char* uplo,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint uplosz,
#endif
                                const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* n,
                                const tint* k,
                                const std::complex<double>* alpha,
                                const std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* b,
                                const tint* ldb,
                                const double* beta,
                                      std::complex<double>* c,
                                const tint* ldc);

// QR subroutines
void CVM_FTN_CALL SGEQRF       (const tint* m,
                                const tint* n,
                                      float* a,
                                const tint* lda,
                                      float* tau,
                                      float* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL DGEQRF       (const tint* m,
                                const tint* n,
                                      double* a,
                                const tint* lda,
                                      double* tau,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL CGEQRF       (const tint* m,
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* tau,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL ZGEQRF       (const tint* m,
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* tau,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL SORGQR       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      float* a,
                                const tint* lda,
                                const float* tau,
                                      float* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL DORGQR       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      double* a,
                                const tint* lda,
                                const double* tau,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL CUNGQR       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* tau,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL ZUNGQR       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* tau,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

// RQ subroutines
void CVM_FTN_CALL SGERQF       (const tint* m,
                                const tint* n,
                                      float* a,
                                const tint* lda,
                                      float* tau,
                                      float* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL DGERQF       (const tint* m,
                                const tint* n,
                                      double* a,
                                const tint* lda,
                                      double* tau,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL CGERQF       (const tint* m,
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* tau,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL ZGERQF       (const tint* m,
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* tau,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);


void CVM_FTN_CALL SORGRQ       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      float* a,
                                const tint* lda,
                                const float* tau,
                                      float* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL DORGRQ       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      double* a,
                                const tint* lda,
                                const double* tau,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL CUNGRQ       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* tau,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL ZUNGRQ       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* tau,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

// LQ subroutines
void CVM_FTN_CALL SGELQF       (const tint* m,
                                const tint* n,
                                      float* a,
                                const tint* lda,
                                      float* tau,
                                      float* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL DGELQF       (const tint* m,
                                const tint* n,
                                      double* a,
                                const tint* lda,
                                      double* tau,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL CGELQF       (const tint* m,
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* tau,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL ZGELQF       (const tint* m,
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* tau,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL SORGLQ       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      float* a,
                                const tint* lda,
                                const float* tau,
                                      float* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL DORGLQ       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      double* a,
                                const tint* lda,
                                const double* tau,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL CUNGLQ       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* tau,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL ZUNGLQ       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* tau,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

// LQ subroutines
void CVM_FTN_CALL SGEQLF       (const tint* m,
                                const tint* n,
                                      float* a,
                                const tint* lda,
                                      float* tau,
                                      float* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL DGEQLF       (const tint* m,
                                const tint* n,
                                      double* a,
                                const tint* lda,
                                      double* tau,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL CGEQLF       (const tint* m,
                                const tint* n,
                                      std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* tau,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL ZGEQLF       (const tint* m,
                                const tint* n,
                                      std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* tau,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL SORGQL       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      float* a,
                                const tint* lda,
                                const float* tau,
                                      float* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL DORGQL       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      double* a,
                                const tint* lda,
                                const double* tau,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL CUNGQL       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      std::complex<float>* a,
                                const tint* lda,
                                const std::complex<float>* tau,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL ZUNGQL       (const tint* m,
                                const tint* n,
                                const tint* k,
                                      std::complex<double>* a,
                                const tint* lda,
                                const std::complex<double>* tau,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

// LLS routines, 6.0
void CVM_FTN_CALL SGELS        (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      float* a,
                                const tint* lda,
                                      float* b,
                                const tint* ldb,
                                      float* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL DGELS        (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      double* a,
                                const tint* lda,
                                      double* b,
                                const tint* ldb,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL CGELS        (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* b,
                                const tint* ldb,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL ZGELS        (const char* trans,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint transsz,
#endif
                                const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* b,
                                const tint* ldb,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL SGELSY       (const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      float* a,
                                const tint* lda,
                                      float* b,
                                const tint* ldb,
                                      tint* jpvt,
                                const float* rcond,
                                      tint* rank,
                                      float* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL DGELSY       (const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      double* a,
                                const tint* lda,
                                      double* b,
                                const tint* ldb,
                                      tint* jpvt,
                                const double* rcond,
                                      tint* rank,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL CGELSY       (const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* b,
                                const tint* ldb,
                                      tint* jpvt,
                                const float* rcond,
                                      tint* rank,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      float* rwork,
                                      tint* info);

void CVM_FTN_CALL ZGELSY       (const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* b,
                                const tint* ldb,
                                      tint* jpvt,
                                const double* rcond,
                                      tint* rank,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      double* rwork,
                                      tint* info);

void CVM_FTN_CALL SGELSS       (const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      float* a,
                                const tint* lda,
                                      float* b,
                                const tint* ldb,
                                      float* s,
                                const float* rcond,
                                      tint* rank,
                                      float* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL DGELSS       (const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      double* a,
                                const tint* lda,
                                      double* b,
                                const tint* ldb,
                                      double* s,
                                const double* rcond,
                                      tint* rank,
                                      double* work,
                                const tint* lwork,
                                      tint* info);

void CVM_FTN_CALL CGELSS       (const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* b,
                                const tint* ldb,
                                      float* s,
                                const float* rcond,
                                      tint* rank,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      float* rwork,
                                      tint* info);

void CVM_FTN_CALL ZGELSS       (const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* b,
                                const tint* ldb,
                                      double* s,
                                const double* rcond,
                                      tint* rank,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      double* rwork,
                                      tint* info);

void CVM_FTN_CALL SGELSD       (const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      float* a,
                                const tint* lda,
                                      float* b,
                                const tint* ldb,
                                      float* s,
                                const float* rcond,
                                      tint* rank,
                                      float* work,
                                const tint* lwork,
                                      tint* iwork,
                                      tint* info);

void CVM_FTN_CALL DGELSD       (const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      double* a,
                                const tint* lda,
                                      double* b,
                                const tint* ldb,
                                      double* s,
                                const double* rcond,
                                      tint* rank,
                                      double* work,
                                const tint* lwork,
                                      tint* iwork,
                                      tint* info);

void CVM_FTN_CALL CGELSD       (const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      std::complex<float>* a,
                                const tint* lda,
                                      std::complex<float>* b,
                                const tint* ldb,
                                      float* s,
                                const float* rcond,
                                      tint* rank,
                                      std::complex<float>* work,
                                const tint* lwork,
                                      float* rwork,
                                      tint* iwork,
                                      tint* info);

void CVM_FTN_CALL ZGELSD       (const tint* m,
                                const tint* n,
                                const tint* nrhs, 
                                      std::complex<double>* a,
                                const tint* lda,
                                      std::complex<double>* b,
                                const tint* ldb,
                                      double* s,
                                const double* rcond,
                                      tint* rank,
                                      std::complex<double>* work,
                                const tint* lwork,
                                      double* rwork,
                                      tint* iwork,
                                      tint* info);

// 8.1 generalized eigenvalues
void CVM_FTN_CALL SGGEV        (const char* jobvl,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobvlsz,
#endif
                                const char* jobvr,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobvrsz,
#endif
                                const tint* n,
                                float* a,
                                const tint* lda,
                                float* b,
                                const tint* ldb,
                                float* alphar,
                                float* alphai,
                                float* beta,
                                float* vl,
                                const tint* ldvl,
                                float* vr,
                                const tint* ldvr,
                                float* work,
                                const tint* lwork,
                                tint* info);

void CVM_FTN_CALL DGGEV        (const char* jobvl,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobvlsz,
#endif
                                const char* jobvr,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobvrsz,
#endif
                                const tint* n,
                                double* a,
                                const tint* lda,
                                double* b,
                                const tint* ldb,
                                double* alphar,
                                double* alphai,
                                double* beta,
                                double* vl,
                                const tint* ldvl,
                                double* vr,
                                const tint* ldvr,
                                double* work,
                                const tint* lwork,
                                tint* info);

void CVM_FTN_CALL CGGEV        (const char* jobvl,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobvlsz,
#endif
                                const char* jobvr,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobvrsz,
#endif
                                const tint* n,
                                std::complex<float>* a,
                                const tint* lda,
                                std::complex<float>* b,
                                const tint* ldb,
                                std::complex<float>* alpha,
                                std::complex<float>* beta,
                                std::complex<float>* vl,
                                const tint* ldvl,
                                std::complex<float>* vr,
                                const tint* ldvr,
                                std::complex<float>* work,
                                const tint* lwork,
                                float* rwork,
                                tint* info);

void CVM_FTN_CALL ZGGEV        (const char* jobvl,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobvlsz,
#endif
                                const char* jobvr,
#if defined (CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                                const tint jobvrsz,
#endif
                                const tint* n,
                                std::complex<double>* a,
                                const tint* lda,
                                std::complex<double>* b,
                                const tint* ldb,
                                std::complex<double>* alpha,
                                std::complex<double>* beta,
                                std::complex<double>* vl,
                                const tint* ldvl,
                                std::complex<double>* vr,
                                const tint* ldvr,
                                std::complex<double>* work,
                                const tint* lwork,
                                double* rwork,
                                tint* info);

#ifdef __cplusplus
}                       // extern "C" 
#endif

#endif                  // !_BLAS_H
