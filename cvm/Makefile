SHELL=/bin/sh

ROOT_PATH=..
INC_PATH=$(ROOT_PATH)/makes
include $(INC_PATH)/makecmd.inc
include $(INC_PATH)/defines.inc

all: debug release

.PHONY: debug
debug:
	$(MAKEUTIL) debug -C ftn $(MAKECMD)
	$(MAKEUTIL) debug -C src $(MAKECMD)
	$(MAKEUTIL) debug -C test $(MAKECMD)

.PHONY: release
release:
	$(MAKEUTIL) release -C ftn $(MAKECMD)
	$(MAKEUTIL) release -C src $(MAKECMD)
	$(MAKEUTIL) release -C test $(MAKECMD)

clean:
	$(MAKEUTIL) -C ftn clean
	$(MAKEUTIL) -C src clean
	$(MAKEUTIL) -C test clean
