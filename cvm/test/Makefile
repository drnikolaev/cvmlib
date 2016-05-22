SHELL=/bin/sh

ROOT_PATH=../..
INC_PATH=$(ROOT_PATH)/makes
include $(INC_PATH)/defines.inc
include $(INC_PATH)/makecmd.inc

.SUFFIXES:

# override ICC
ifdef ICCT
  CC=icpc
ifndef DEBUG
  COMMON_FLAGS += -O0 #intel's glitch
endif
else
ifdef MAC
  CC=clang++
else
  CC=g++
endif
endif

COMMON_FLAGS += -Wall -D_REENTRANT -I../../gtest/googletest/include

ifdef CVM_FLOAT
  COMMON_FLAGS += -DCVM_FLOAT
endif
ifdef CVM_ZERO_BASED
  COMMON_FLAGS += -DCVM_ZERO_BASED
endif
ifdef VERBOSE
#  COMMON_FLAGS += -v
endif

ifdef MAC
  COMMON_FLAGS += -DMAC
endif

ifdef COMSPEC
# Cygwin
  STATIC_ONLY=1
else
  COMMON_FLAGS += -pthread -fPIC
#  LIBS+=-lpthread
#  DLIBS+=-lpthread
endif

ifdef CVM_STLPORT
  COMMON_FLAGS += -I/usr/include/stlport
  LIBS  += -lstlport
  DLIBS += -lstlportg
endif

ifdef ICCT
  COMMON_FLAGS += -I/usr/include/i386-linux-gnu
else
  LIBS  += -lgfortran
  DLIBS += -lgfortran
endif

LIBS  += -lgtest
DLIBS += -lgtest

COMMON_FLAGS += -std=c++0x
ifdef MAC
COMMON_LD_FLAGS=-L$(OUTDIR) -L$(OUTDIR)/../gtest -L/usr/local/lib
else
COMMON_LD_FLAGS=-L$(OUTDIR) -L$(OUTDIR)/../gtest -L/usr/local/lib/gcc46 -L/usr/local/lib/gcc47 -L/usr/local/lib/gcc48
endif

include $(INC_PATH)/suffixes.inc

ifdef STATIC_ONLY
  LIBS  +=$(call concat,-lftn,$(NF),$(NM),$(NE),$(NI))
  DLIBS +=$(call concat,-lftn,$(NF),$(NM),$(NE),$(NI),_debug)

  ifdef IFORT
    include $(INC_PATH)/ifort.inc
  endif
  include $(INC_PATH)/libs.inc
endif


DOBJDIR=./dobj
OBJDIR=./obj

LIBS  +=$(call concat,-lcvm,$(NE),$(NI))
DLIBS +=$(call concat,-lcvm,$(NE),$(NI),_debug)
OUTNAME_RELEASE =$(call concat,regtest_cvm,$(NE),$(NI))
OUTNAME_DEBUG   =$(call concat,regtest_cvm,$(NE),$(NI),_debug)

SOURCES=$(wildcard *.cpp)
OBJS=$(patsubst %.cpp, $(OBJDIR)/%.o, $(SOURCES))
DOBJS=$(patsubst %.cpp, $(DOBJDIR)/%.o, $(SOURCES))


CFLAGS=$(COMMON_FLAGS)
CFLAGSD=$(COMMON_FLAGS) -g -DDEBUG
LFLAGS=$(COMMON_LD_FLAGS)
LFLAGSD=$(COMMON_LD_FLAGS)

ifdef PROF
  CFLAGSD += -pg
  LFLAGSD += -pg
endif

ifdef STATIC_ONLY
ifndef MKL
ifdef GF_BLAS
  ifdef MAC
    BLAS_PATH = mac/gf  
  else
    BLAS_PATH = linux/gf
  endif
else
  ifdef MAC
    BLAS_PATH = mac/ifort
  else
    BLAS_PATH = linux/ifort
  endif
endif
endif
LFLAGS  += -L$(OUTDIR)/$(BLAS_PATH)
LFLAGSD += -L$(OUTDIR)/$(BLAS_PATH)
endif

LIBS  += -lpthread
DLIBS += -lpthread


all: debug release
release : $(OUTDIR)/$(OUTNAME_RELEASE)
debug   : $(OUTDIR)/$(OUTNAME_DEBUG)

ifdef MAC
$(OUTDIR)/$(OUTNAME_RELEASE) : $(OBJS)
	$(ECHO) Linking: $@
	$(AT)$(CC) -o $@ $(OBJS) $(LFLAGS) $(LIBS)

$(OUTDIR)/$(OUTNAME_DEBUG) : $(DOBJS)
	$(ECHO) Linking: $@
	$(AT)$(CC) -o $@ $(DOBJS) $(LFLAGSD) $(DLIBS)
else
$(OUTDIR)/$(OUTNAME_RELEASE) : $(OBJS)
	$(ECHO) Linking: $@
	$(AT)$(CC) -o $@ $(OBJS) $(LFLAGS) -Wl,--start-group $(LIBS) -Wl,--end-group

$(OUTDIR)/$(OUTNAME_DEBUG) : $(DOBJS)
	$(ECHO) Linking: $@
	$(AT)$(CC) -o $@ $(DOBJS) $(LFLAGSD) -Wl,--start-group $(DLIBS) -Wl,--end-group
endif

$(OBJDIR)/%.o: %.cpp
	$(AT)if [ ! -d $(OBJDIR) ]; then mkdir $(OBJDIR); fi
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	$(ECHO) Compiling: $<
	$(AT)$(CC) $(CFLAGS) -c -o $@ $<

$(DOBJDIR)/%.o: %.cpp
	$(AT)if [ ! -d $(DOBJDIR) ]; then mkdir $(DOBJDIR); fi
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	$(ECHO) Compiling: $<
	$(AT)$(CC) $(CFLAGSD) -c -o $@ $<

clean:
	$(ECHO) Cleaning...
	$(AT)-rm -rf $(OBJDIR) $(DOBJDIR) $(OUTDIR)/$(OUTNAME_RELEASE) $(OUTDIR)/$(OUTNAME_DEBUG) $(OUTDIR)/*testout*.txt

ifneq "$(MAKECMDGOALS)" "clean"
  -include $(OBJDIR)/*.d $(DOBJDIR)/*.d
endif

