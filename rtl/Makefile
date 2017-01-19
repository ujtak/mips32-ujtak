#### notice, don't forget update Makefile.dep
# SIMULATOR = (vsim,) vcs, xsim, iverilog
SIMULATOR = iverilog
MODULE    = top
TEST_DIR  = test
OUTFILE   = mem_data

LINK_DIR  = .
DIST      = ../dist
TARGET    = $(TEST_DIR)/test_$(MODULE)

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
	nvim -O $(TARGET).v.erb `echo $(TARGET) | awk -F "$(TEST_DIR)/test_" '{print $$2 ".v.erb"}'`

diff:
	nvim -d $(OUTFILE)_test.dat $(OUTFILE)_true.dat

define DEP_IN
## -*- text -*-
## ignore list

ignore_file {
}

ignore_com {
}
endef
export DEP_IN
