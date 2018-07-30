SHELL=/bin/sh

ROOT_PATH=../..
INC_PATH=$(ROOT_PATH)/makes
include $(INC_PATH)/defines.inc
include $(INC_PATH)/makecmd.inc

ifneq (cygwin,${TERM})
#ifndef COMSPEC
  FCOMMON_FLAGS += -fPIC
endif

SOURCES = $(wildcard ?????.f)
# some stuff missing in libblas.a
ifndef MKL
  SOURCES += $(wildcard ??????.f)
endif
OBJS  = $(patsubst %.f, $(OBJDIR)/%.o, $(SOURCES))
DOBJS = $(patsubst %.f, $(DOBJDIR)/%.o, $(SOURCES))

include $(INC_PATH)/suffixes.inc

FFLAGS  = -O3 $(FCOMMON_FLAGS)
FFLAGSD = -g $(FCOMMON_FLAGS)

LIBNAME  =$(call concat,libftn,$(NF),$(NM),$(NE),$(NI),.a)
DLIBNAME =$(call concat,libftn,$(NF),$(NM),$(NE),$(NI),_debug.a)
OBJDIR   =$(call concat,./obj,$(NF),$(NM),$(NE),$(NI))
DOBJDIR  =$(call concat,./dobj,$(NF),$(NM),$(NE),$(NI))

all:     debug release
release: $(OUTDIR)/$(LIBNAME)
debug:   $(OUTDIR)/$(DLIBNAME)

$(OUTDIR)/$(LIBNAME): $(OBJS)
	$(ECHO) Archiving: $@
	$(AT)$(AR) $@ $(OBJS)

$(OUTDIR)/$(DLIBNAME): $(DOBJS)
	$(ECHO) Archiving: $@
	$(AT)$(AR) $@ $(DOBJS)

$(OBJDIR)/%.o: %.f
	$(AT)if [ ! -d $(OBJDIR) ]; then mkdir $(OBJDIR); fi
	$(ECHO) Compiling release: $<
	$(AT)$(F77) $(FFLAGS) -c -o $@ $<

$(DOBJDIR)/%.o: %.f
	$(AT)if [ ! -d $(DOBJDIR) ]; then mkdir $(DOBJDIR); fi
	$(ECHO) Compiling debug: $<
	$(AT)$(F77) $(FFLAGSD) -c -o $@ $<

clean:
	$(ECHO) Cleaning...
	$(AT)rm -rf $(OBJDIR) $(DOBJDIR)
	$(AT)rm -f $(OUTDIR)/$(LIBNAME) $(OUTDIR)/$(DLIBNAME)

