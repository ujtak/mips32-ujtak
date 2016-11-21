#### notice, don't forget update Makefile.dep
#### (make compile.all USE_CACHE=0; ...; rm -f Makefile.dep; make Makefile.dep)
# SIMULATOR=vcs
# SIMULATOR=iverilog
# SIMULATOR=xsim
IOC_DIR=.
TARGET=test_alu
DIST=../dist

include Makefile.dep
include VERBMakefile

.PHONY: all file sim clean
sim: $(TARGET).log
file: $(TARGET).com

all:
	make clean
	make sim

clean:
	make clean-common
	rm -f log.txt
	rm -f vish_stacktrace.vstf
	rm -f $(TARGET)

log:
	vim $(TARGET).log

edit:
	vim -O $(TARGET).v.erb `echo $(TARGET) | awk -F "test_" '{print $$2 ".v.erb"}'`

diff:
	vimdiff $(TARGET).dat $(TARGET)_true.dat

define DEP_IN
## -*- text -*-
## ignore list

ignore_file {
}

ignore_com {
}
endef
export DEP_IN
