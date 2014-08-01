ifndef MATLAB_PATH
  MATLAB_PATH=/opt/MATLAB_R2009B
endif
ifdef EM64T
  COMMON_LD_FLAGS += -L$(MATLAB_PATH)/bin/glnxa64
else
  COMMON_LD_FLAGS += -L$(MATLAB_PATH)/bin/glnx86
endif

FCOMMON_FLAGS += -fPIC -fno-omit-frame-pointer
LIBS  += -lmx -lmex -lmat
DLIBS += -lmx -lmex -lmat

