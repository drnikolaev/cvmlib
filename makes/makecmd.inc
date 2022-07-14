ifdef MAC
  MAKECMD += MAC=1
else
  UNAME_S := $(shell uname -s)
  ifeq ($(UNAME_S),Darwin)
    MAC=1
    MAKECMD += MAC=1
  endif
endif
ifdef IFORT
  MAKECMD += IFORT=1
endif
ifdef ICC
  MAKECMD += ICC=1
endif
ifdef ICCT
  MAKECMD += ICCT=1
endif
ifdef MKL
  MAKECMD += MKL=1
endif
ifdef CVM_FLOAT
  MAKECMD += CVM_FLOAT=1
endif

ifdef EM64T
  MAKECMD += EM64T=1
else
  LBITS := $(shell getconf LONG_BIT)
  ifeq ($(LBITS),64)
    EM64T=1
    MAKECMD += EM64T=1
  endif
endif

ifdef ILP64
  MAKECMD += ILP64=1
endif
#ifdef MKL_PATH
#  MAKECMD += MKL_PATH=$(MKL_PATH)
#endif
ifdef STATIC_ONLY
  MAKECMD += STATIC_ONLY=1
endif
ifdef CVM_NO_MT
  MAKECMD += CVM_NO_MT=1
endif
ifdef CVM_USE_POOL_MANAGER
  MAKECMD += CVM_USE_POOL_MANAGER=1
endif
ifdef GF_BLAS=1
  MAKECMD += GF_BLAS=1
else
  ifndef IFORT
    GF_BLAS=1
    MAKECMD += GF_BLAS=1
  endif
endif

