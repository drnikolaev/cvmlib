ifdef ACML
  ifndef ACML_PATH
    ifdef EM64T
      ACML_PATH=/opt/acml5.3.1
    else
      ACML_PATH=/opt/acml4.4.0
    endif
  endif
  ifdef ICC
    ifdef EM64T
      ifdef ILP64
        ACML_PATHL=$(ACML_PATH)/ifort64_int64/lib/
      else
        ACML_PATHL=$(ACML_PATH)/ifort64/lib/
      endif
    else
      ACML_PATHL=$(ACML_PATH)/ifort32/lib/
    endif
  else
    ifdef IFORT
      ifdef EM64T
        ifdef ILP64
          ACML_PATHL=$(ACML_PATH)/ifort64_int64/lib/
        else
          ACML_PATHL=$(ACML_PATH)/ifort64/lib/
        endif
      else
        ACML_PATHL=$(ACML_PATH)/ifort32/lib/
      endif
    else
      ifdef EM64T
        ifdef ILP64
          ACML_PATHL=$(ACML_PATH)/gfortran64_int64/lib/
        else
          ACML_PATHL=$(ACML_PATH)/gfortran64/lib/
        endif
      else
        ACML_PATHL=$(ACML_PATH)/gfortran32/lib/
      endif
    endif
  endif
  LIBS  += -lacml
  DLIBS += -lacml
  COMMON_FLAGS += -DCVM_ACML=1
  COMMON_LD_FLAGS += -L$(ACML_PATHL) -Wl,-rpath,$(ACML_PATHL)
#  COMMON_LD_FLAGS += -L$(ACML_PATHL)
else 
ifdef ACML_MP
  ifndef ACML_PATH
    ifdef EM64T
      ACML_PATH=/opt/acml5.3.1
    else
      ACML_PATH=/opt/acml4.4.0
    endif
  endif
  ifdef ICC
    ifdef EM64T
      ifdef ILP64
        ACML_PATHL=$(ACML_PATH)/ifort64_mp_int64/lib/
      else
        ACML_PATHL=$(ACML_PATH)/ifort64_mp/lib/
      endif
    else
      ACML_PATHL=$(ACML_PATH)/ifort32_mp/lib/
    endif
  else
    ifdef IFORT
      ifdef EM64T
        ifdef ILP64
          ACML_PATHL=$(ACML_PATH)/ifort64_mp_int64/lib/
        else
          ACML_PATHL=$(ACML_PATH)/ifort64_mp/lib/
        endif
      else
        ACML_PATHL=$(ACML_PATH)/ifort32_mp/lib/
      endif
    else
      ifdef EM64T
        ifdef ILP64
          ACML_PATHL=$(ACML_PATH)/gfortran64_mp_int64/lib/
        else
          ACML_PATHL=$(ACML_PATH)/gfortran64_mp/lib/
        endif
      else
        ACML_PATHL=$(ACML_PATH)/gfortran32_mp/lib/
      endif
    endif
  endif
  LIBS  += -lacml_mp
  DLIBS += -lacml_mp
  ifdef ICC
    LIBS  += -liomp5
    DLIBS += -liomp5
  endif
  COMMON_FLAGS += -DCVM_ACML=1
  COMMON_LD_FLAGS += -L$(ACML_PATHL) -Wl,-rpath,$(ACML_PATHL)
#  COMMON_LD_FLAGS += -L$(ACML_PATHL)
else
ifdef MKL
  ifndef MKL_PATH
    MKL_PATH=/opt/intel/composerxe/mkl/lib
  endif
  ifdef EM64T
    MKL_PATHL=$(MKL_PATH)/intel64
    ifdef ICC
      MKL_LIBS  += -lmkl_intel_thread
      ifdef ILP64
        MKL_LIBS  += -lmkl_intel_ilp64 
      else 
        MKL_LIBS  += -lmkl_intel_lp64
      endif
    else
      ifdef IFORT
        MKL_LIBS  += -lmkl_intel_thread
        ifdef ILP64
          MKL_LIBS  += -lmkl_intel_ilp64 
        else 
          MKL_LIBS  += -lmkl_intel_lp64
        endif
      else
        MKL_LIBS  += -lmkl_gnu_thread
        ifdef ILP64
          MKL_LIBS  += -lmkl_gf_ilp64 
        else 
          MKL_LIBS  += -lmkl_gf_lp64
        endif
      endif
    endif
    MKL_LIBS += -lmkl_sequential -lmkl_core -liomp5
  else
    MKL_PATHL=$(MKL_PATH)/ia32
    MKL_LIBS += -liomp5
    ifdef ICC
      MKL_LIBS += -lmkl_intel_thread -lmkl_intel
    else
      ifdef IFORT
        MKL_LIBS += -lmkl_intel_thread -lmkl_intel
      else
        MKL_LIBS += -lmkl_gnu_thread -lmkl_gf
      endif
    endif
    MKL_LIBS += -lmkl_sequential -lmkl_core
  endif
#  COMMON_LD_FLAGS += -L$(MKL_PATHL) -Wl,-rpath,$(MKL_PATHL)
  COMMON_LD_FLAGS += -L$(MKL_PATHL)
  LIBS  += $(MKL_LIBS)
  DLIBS += $(MKL_LIBS)
else
  LIBS  += $(call concat,-lblas,$(NF),$(NE),$(NI))
  LIBS  += $(call concat,-llapack,$(NF),$(NE),$(NI))
  DLIBS += $(call concat,-lblas,$(NF),$(NE),$(NI),_debug)
  DLIBS += $(call concat,-llapack,$(NF),$(NE),$(NI),_debug)
endif
endif
endif

