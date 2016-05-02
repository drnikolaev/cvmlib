ifdef ICC
  NC=_icc
else
  NC=_gcc
endif
ifdef GF_BLAS
  NF=
else
  NF=_ifort
endif
ifdef MKL
  NM=_mkl
endif
ifdef EM64T
  NE=_em64t
  OUTDIR  = $(ROOT_PATH)/lib64
else  
  NE=_ia32
  OUTDIR  = $(ROOT_PATH)/lib
endif
ifdef ILP64
  ifdef EM64T
    COMMON_FLAGS+=-DCVM_ILP64
    NI=_ilp64
  endif
endif

