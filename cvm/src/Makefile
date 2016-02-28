SHELL=/bin/sh

ROOT_PATH=../..
INC_PATH=$(ROOT_PATH)/makes
include $(INC_PATH)/defines.inc

ifdef MAC
COMMON_LD_FLAGS = -L/usr/local/lib
else
COMMON_LD_FLAGS = -L/usr/local/lib/gcc46 -L/usr/local/lib/gcc47 -L/usr/local/lib/gcc48
endif

ifdef CVM_USE_POOL_MANAGER
  COMMON_FLAGS += -DCVM_USE_POOL_MANAGER
endif

ifdef CVM_ZERO_BASED
  COMMON_FLAGS += -DCVM_ZERO_BASED
endif

include $(INC_PATH)/libs.inc
# gfortran must be the las one starting from gcc 4.6.1
ifndef IFORT
  LIBS  += -lgfortran
  DLIBS += -lgfortran
endif

ifdef COMSPEC
#  last cygwin is ok, keeping just in case:
#  LIBS  += -lftn_cygwin_missing
#  DLIBS += -lftn_cygwin_missing
else
  #ifeq ($(OSTYPE),linux)
  LIBS  += -lpthread
  DLIBS += -lpthread
  COMMON_FLAGS += -pthread
endif

# hot fix for compatibility with gcc includes
ifdef ICC
  COMMON_FLAGS += -I/usr/include/i386-linux-gnu -I/usr/include/i386-linux-gnu/c++/4.8 -I/usr/include/x86_64-linux-gnu/c++/4.8
endif

include $(INC_PATH)/suffixes.inc

FLIBDIR=../ftn
FLIB      =$(call concat,libftn,$(NF),$(NM),$(NE),$(NI),.a)
DFLIB     =$(call concat,libftn,$(NF),$(NM),$(NE),$(NI),_debug.a)

OBJDIR    =$(call concat,./obj,$(NC),$(NM),$(NE),$(NI))
DOBJDIR   =$(call concat,./dobj,$(NC),$(NM),$(NE),$(NI))
SOOBJDIR  =$(call concat,./soobj,$(NC),$(NM),$(NE),$(NI))
SODOBJDIR =$(call concat,./sodobj,$(NC),$(NM),$(NE),$(NI))

SOURCES   =$(wildcard *.cpp)
OBJS      =$(patsubst %.cpp, $(OBJDIR)/%.o, $(SOURCES))
DOBJS     =$(patsubst %.cpp, $(DOBJDIR)/%.o, $(SOURCES))
SOOBJS    =$(patsubst %.cpp, $(SOOBJDIR)/%.o, $(SOURCES))
SODOBJS   =$(patsubst %.cpp, $(SODOBJDIR)/%.o, $(SOURCES))

SONAME    =$(call concat,libcvm,$(NE),$(NI),.so)
SODNAME   =$(call concat,libcvm,$(NE),$(NI),_debug.so)
DYNAME    =$(call concat,libcvm,$(NE),$(NI),.dylib)
DYDNAME   =$(call concat,libcvm,$(NE),$(NI),_debug.dylib)
LIBNAME   =$(call concat,libcvm,$(NE),$(NI),.a)
LIBDNAME  =$(call concat,libcvm,$(NE),$(NI),_debug.a)

ifdef IFORT
  include $(INC_PATH)/ifort.inc
endif

COMMON_FLAGS += -Wall
ifdef CVM_FLOAT
  COMMON_FLAGS += -DCVM_FLOAT
endif

ifdef MAC
  COMMON_FLAGS += -DMAC
endif

ifdef CVM_STLPORT
  COMMON_FLAGS += -I/usr/include/stlport
  LIBS  += -lstlport
  DLIBS += -lstlportg
endif

COMMON_FLAGS += -std=c++0x

CFLAGS    =$(COMMON_FLAGS) -O3
CFLAGSD   =$(COMMON_FLAGS) -g -DDEBUG
SOCFLAGS  =$(CFLAGS) -D_REENTRANT -DCVM_EXPORTS
SOCFLAGSD =$(CFLAGSD) -D_REENTRANT -DCVM_EXPORTS

ifndef COMSPEC
  SOCFLAGS += -fPIC
  SOCFLAGSD += -fPIC
endif

LFLAGS  += $(COMMON_LD_FLAGS) -L$(OUTDIR)
LFLAGSD += $(COMMON_LD_FLAGS) -L$(OUTDIR)

ifndef MKL
ifdef IFORT
  LFLAGS  += -L$(OUTDIR)/linux/ifort
  LFLAGSD += -L$(OUTDIR)/linux/ifort
else
  LFLAGS  += -L$(OUTDIR)/linux/gf
  LFLAGSD += -L$(OUTDIR)/linux/gf
endif
endif
LFLAGS  += -shared
LFLAGSD += -shared


ifdef PROF
  CFLAGSD += -pg
  LFLAGSD += -pg
endif

all : release debug
ifdef STATIC_ONLY
  release: $(OUTDIR)/$(LIBNAME)
  debug:   $(OUTDIR)/$(LIBDNAME)
else
  ifdef MAC
    release: $(OUTDIR)/$(LIBNAME) $(OUTDIR)/$(DYNAME)
    debug:   $(OUTDIR)/$(LIBDNAME) $(OUTDIR)/$(DYDNAME)
  else
    release: $(OUTDIR)/$(LIBNAME) $(OUTDIR)/$(SONAME)
    debug:   $(OUTDIR)/$(LIBDNAME) $(OUTDIR)/$(SODNAME)
  endif
endif

$(OUTDIR)/$(LIBNAME): $(OBJS)
	$(ECHO) Archiving: $@
	$(AT)$(AR) $@ $^

$(OUTDIR)/$(LIBDNAME): $(DOBJS)
	$(ECHO) Archiving: $@
	$(AT)$(AR) $@ $^

$(OUTDIR)/$(FLIB):
	make release -C $(FLIBDIR) $(MAKECMD)

$(OUTDIR)/$(DFLIB):
	make debug -C $(FLIBDIR) $(MAKECMD)

$(OUTDIR)/$(SONAME): $(SOOBJS) $(OUTDIR)/$(FLIB)
	$(AT)if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	$(ECHO) Linking: $@
	$(AT)$(CC) $(LFLAGS) -Wl,-soname,$(SONAME) -o $@ $^ -Wl,--start-group $(LIBS) -Wl,--end-group

$(OUTDIR)/$(SODNAME): $(SODOBJS) $(OUTDIR)/$(DFLIB)
	$(AT)if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	$(ECHO) Linking: $@
	$(AT)$(CC) $(LFLAGSD) -Wl,-soname,$(SODNAME) -o $@ $^ -Wl,--start-group $(DLIBS) -Wl,--end-group

$(OUTDIR)/$(DYNAME): $(SOOBJS) $(OUTDIR)/$(FLIB)
	$(AT)if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	$(ECHO) Linking: $@
	$(AT)$(CC) $(LFLAGS) -Wl,-dylib -o $@ $^ $(LIBS)

$(OUTDIR)/$(DYDNAME): $(SODOBJS) $(OUTDIR)/$(DFLIB)
	$(AT)if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	$(ECHO) Linking: $@
	$(AT)$(CC) $(LFLAGSD) -Wl,-dylib -o $@ $^ $(DLIBS)

$(OBJDIR)/%.o: %.cpp
	$(AT)if [ ! -d $(OBJDIR) ]; then mkdir $(OBJDIR); fi
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	$(ECHO) Compiling release: $<
	$(AT)$(CC) $(CFLAGS) -c -o $@ $<

$(DOBJDIR)/%.o: %.cpp
	$(AT)if [ ! -d $(DOBJDIR) ]; then mkdir $(DOBJDIR); fi
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	$(ECHO) Compiling debug: $<
	$(AT)$(CC) $(CFLAGSD) -c -o $@ $<

$(SOOBJDIR)/%.o: %.cpp
	$(AT)if [ ! -d $(SOOBJDIR) ]; then mkdir $(SOOBJDIR); fi
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	$(ECHO) Compiling release: $<
	$(AT)$(CC) $(SOCFLAGS) -c -o $@ $<

$(SODOBJDIR)/%.o: %.cpp
	$(AT)if [ ! -d $(SODOBJDIR) ]; then mkdir $(SODOBJDIR); fi
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	$(ECHO) Compiling debug: $<
	$(AT)$(CC) $(SOCFLAGSD) -c -o $@ $<

clean:
	$(ECHO) Cleaning...
	$(AT)rm -rf $(OBJDIR) $(DOBJDIR) $(SOOBJDIR) $(SODOBJDIR) $(OUTDIR)/$(SONAME) $(OUTDIR)/$(SODNAME) $(OUTDIR)/$(LIBNAME) $(OUTDIR)/$(LIBDNAME)

ifneq "$(MAKECMDGOALS)" "clean"
  -include $(OBJDIR)/*.d $(DOBJDIR)/*.d $(SOOBJDIR)/*.d $(SODOBJDIR)/*.d
endif

