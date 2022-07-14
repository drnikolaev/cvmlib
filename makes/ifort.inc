LIBS  += -lifport -lifcoremt -limf -lirc -lsvml -ldl
DLIBS += -lifport -lifcoremt -limf -lirc -lsvml -ldl
ifndef IFORT_PATH
  IFORT_PATH=/opt/intel
endif
ifdef EM64T
  ifdef MAC
    COMMON_LD_FLAGS += -L$(IFORT_PATH)/lib
  else
    COMMON_LD_FLAGS += -L$(IFORT_PATH)/lib/intel64
  endif
else
  COMMON_LD_FLAGS += -L$(IFORT_PATH)/lib/ia32
endif

