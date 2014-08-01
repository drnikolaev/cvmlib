LIBS  += -lifport -lifcoremt -limf -lirc -lsvml -ldl
DLIBS += -lifport -lifcoremt -limf -lirc -lsvml -ldl
ifndef IFORT_PATH
  IFORT_PATH=/opt/intel/composerxe/bin
endif
ifdef EM64T
  COMMON_LD_FLAGS += -L$(IFORT_PATH)/lib/intel64
else 
  COMMON_LD_FLAGS += -L$(IFORT_PATH)/lib/ia32
endif

