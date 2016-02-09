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
  COMMON_LD_FLAGS += -L$(MKL_PATHL)
  LIBS  += $(MKL_LIBS)
  DLIBS += $(MKL_LIBS)
else
  LIBS  += $(call concat,-lblas,$(NF),$(NE),$(NI))
  LIBS  += $(call concat,-llapack,$(NF),$(NE),$(NI))
  DLIBS += $(call concat,-lblas,$(NF),$(NE),$(NI),_debug)
  DLIBS += $(call concat,-llapack,$(NF),$(NE),$(NI),_debug)
endif

