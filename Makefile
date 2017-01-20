framework_dir = $(abspath ./dsp-framework)
base_dir = $(abspath .)

ROCKETCHIP_DIR=$(framework_dir)/rocket-chip

include $(framework_dir)/Makefrag

FIRRTL_JAR ?= $(ROCKETCHIP_DIR)/firrtl/utils/bin/firrtl.jar
FIRRTL ?= java -Xmx2G -Xss8M -cp $(FIRRTL_JAR) firrtl.Driver

CHISEL_ARGS ?= 
build_dir ?= generated-src
PROJECT ?= fft
MODEL ?= TestHarness
CFG_PROJECT ?= $(PROJECT)
CONFIG ?= DspConfig


$(build_dir)/$(PROJECT).$(MODEL).$(CONFIG).fir: $(rocketchip_stamp) $(extra_stamps) $(call lookup_scala_srcs,$(base_dir)/src/main/scala)
	mkdir -p $(build_dir)
	cd $(base_dir) && $(SBT) "run-main $(PROJECT).Generator $(CHISEL_ARGS) $(build_dir) $(PROJECT) $(MODEL) $(CFG_PROJECT) $(CONFIG)"

$(build_dir)/$(PROJECT).$(MODEL).$(CONFIG).v: $(build_dir)/$(PROJECT).$(MODEL).$(CONFIG).fir
	$(FIRRTL) -i $< -o $@ -X verilog

verilog: $(build_dir)/$(PROJECT).$(MODEL).$(CONFIG).v

test: $(all_stamps)
	$(SBT) test

travis: $(all_stamps)
	$(SBT) travis:test

pages: $(all_stamps)
	$(SBT) ghpagesPushSite
