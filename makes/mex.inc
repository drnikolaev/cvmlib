DES_DIR := $(OUTDIR)
COMMON_LD_FLAGS += -L$(DES_DIR)
# then overriding previously defined
OUTDIR=.
DOUTDIR=Debug

SOURCES =$(wildcard *.f)
UTILSOURCES=$(wildcard ../utils/*.f)
SOURCES+=$(subst ../utils/,, $(UTILSOURCES))
OBJS    =$(patsubst %.f, $(OBJDIR)/%.o, $(SOURCES))
DOBJS   =$(patsubst %.f, $(DOBJDIR)/%.o, $(SOURCES))
OBJDIR  =$(call concat,./obj,$(NF),$(NM),$(NE),$(NI))
DOBJDIR =$(call concat,./dobj,$(NF),$(NM),$(NE),$(NI))

include $(INC_PATH)/libs.inc

ifdef IFORT
  include $(INC_PATH)/ifort.inc
else
  LIBS  += -lgfortran
  DLIBS += -lgfortran
endif

SOFLAGS  = $(FCOMMON_FLAGS) -O3
SOFLAGSD = $(FCOMMON_FLAGS) -g
LFLAGS   = $(COMMON_LD_FLAGS) -shared
LFLAGSD  = $(COMMON_LD_FLAGS) -shared

all : release
release: $(OUTDIR)/$(SONAME)
debug:   $(DOUTDIR)/$(SONAME)

$(OUTDIR)/$(SONAME): $(OBJS)
	$(AT)if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	$(ECHO) Linking: $@
	$(AT)$(F77) $(LFLAGS) -Wl,-soname,$(SONAME) -o $@ $^ -Wl,--start-group $(LIBS) -Wl,--end-group

$(DOUTDIR)/$(SONAME): $(DOBJS)
	$(AT)if [ ! -d $(DOUTDIR) ]; then mkdir $(DOUTDIR); fi
	$(ECHO) Linking: $@
	$(AT)$(F77) $(LFLAGSD) -Wl,-soname,$(SONAME) -o $@ $^ -Wl,--start-group $(DLIBS) -Wl,--end-group

$(OBJDIR)/%.o: %.f
	$(AT)if [ ! -d $(OBJDIR) ]; then mkdir $(OBJDIR); fi
	$(ECHO) Compiling release: $<
	$(AT)$(F77) $(SOFLAGS) -c -o $@ $<

$(DOBJDIR)/%.o: %.f
	$(AT)if [ ! -d $(DOBJDIR) ]; then mkdir $(DOBJDIR); fi
	$(ECHO) Compiling debug: $<
	$(AT)$(F77) $(SOFLAGSD) -c -o $@ $<

$(OBJDIR)/%.o: ../utils/%.f
	$(AT)if [ ! -d $(OBJDIR) ]; then mkdir $(OBJDIR); fi
	$(ECHO) Compiling release: $<
	$(AT)$(F77) $(SOFLAGS) -c -o $@ $<

$(DOBJDIR)/%.o: ../utils/%.f
	$(AT)if [ ! -d $(DOBJDIR) ]; then mkdir $(DOBJDIR); fi
	$(ECHO) Compiling debug: $<
	$(AT)$(F77) $(SOFLAGSD) -c -o $@ $<

clean:
	$(ECHO) Cleaning...
	$(AT)rm -rf $(OBJDIR) $(DOBJDIR) $(OUTDIR)/$(SONAME) $(DOUTDIR)/$(SONAME)

