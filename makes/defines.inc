.SUFFIXES:

#OS detection: http://stackoverflow.com/questions/714100/os-detecting-makefile

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),FreeBSD)
  MAKEUTIL = gmake
else
  MAKEUTIL = make
endif

ifdef VERBOSE
AT=
else
AT=@
endif

AR = ar cr
ECHO = @echo
ifdef ICC
  CC = icpc
else
  ifeq ($(UNAME_S),$(filter $(UNAME_S),FreeBSD Darwin))
    CC = clang++
  else
    CC = g++
  endif
endif

ifdef IFORT
  F77 = ifort
  FCOMMON_FLAGS += -threads
else
  F77 = gfortran
endif

ifdef ILP64
  ifdef EM64T
    ifdef IFORT
      FCOMMON_FLAGS += -i8
    else
      FCOMMON_FLAGS += -fdefault-integer-8
    endif
    NI=_ilp64
  endif
endif


define concat
$1$2$3$4$5$6$7$8
endef

#$(call make-depend,source-file,object-file,depend-file)
define make-depend
  $(AT)$(CC) -MM -MF $3 -MP -MT $2 $(COMMON_FLAGS) $1
endef

