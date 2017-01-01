#### notice, don't forget update Makefile.dep
#### (make compile.all USE_CACHE=0; ...; rm -f Makefile.dep; make Makefile.dep)
# SIMULATOR=vcs
SIMULATOR = iverilog
# SIMULATOR=xsim
LINK_DIR = .
TEST_DIR = tests
MODULE   = sign_ext
TARGET   = $(TEST_DIR)/test_$(MODULE)
DIST     = ../dist

include Makefile.dep
include VERBMakefile

.PHONY: all file sim clean log edit diff
file: $(TARGET).com
sim: $(TARGET).log

all:
	make clean
	make sim

clean: clean-common

log:
	nvim $(TARGET).log

edit:
	nvim -O $(TARGET).v.erb `echo $(TARGET) | awk -F "$(TEST__DIR)/test_" '{print $$2 ".v.erb"}'`

diff:
	nvim -d $(TARGET).dat $(TARGET)_true.dat

define DEP_IN
## -*- text -*-
## ignore list

ignore_file {
}

ignore_com {
}
endef
export DEP_IN
