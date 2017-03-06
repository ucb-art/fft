base_dir ?= $(abspath .)
lib_dir = $(base_dir)/lib
framework_dir = $(base_dir)/dsp-framework
ivy_dir = $(base_dir)/.ivy2
ROCKETCHIP_DIR=$(framework_dir)/rocket-chip
TESTCHIPIP_DIR=$(framework_dir)/testchipip

SBT ?= java -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -Xmx2G -Xss128M -jar $(ROCKETCHIP_DIR)/sbt-launch.jar -Dsbt.ivy.home="${ivy_dir}"

include $(framework_dir)/Makefrag

FIRRTL_JAR ?= $(ROCKETCHIP_DIR)/firrtl/utils/bin/firrtl.jar
FIRRTL ?= java -Xmx2G -Xss8M -cp $(FIRRTL_JAR) firrtl.Driver

CHISEL_ARGS ?= 
build_dir ?= generated-src
PROJECT ?= craft
MODEL ?= DspTop
CFG_PROJECT ?= fft
CONFIG ?= DefaultStandaloneFixedPointFFTConfig
VLSITOP ?= FFTBlock

long_name = $(PROJECT).$(MODEL).$(CONFIG)

$(build_dir)/$(long_name).fir: $(call lookup_scala_srcs, $(base_dir)/src) $(all_stamps)
	mkdir -p $(build_dir)
	cd $(base_dir) && $(SBT) "run-main $(PROJECT).Generator $(CHISEL_ARGS) $(build_dir) $(PROJECT) $(MODEL) $(CFG_PROJECT) $(CONFIG)"

$(build_dir)/$(long_name).top.v $(build_dir)/$(long_name).harness.v: $(build_dir)/$(long_name).fir $(FIRRTL_JAR)
	cd $(base_dir) && $(SBT) "run-main barstools.tapeout.transforms.GenerateTopAndHarness -i $< --top-o $(build_dir)/$(long_name).top.v --harness-o $(build_dir)/$(long_name).harness.v --syn-top $(VLSITOP) --harness-top $(MODEL)"

firrtl: $(build_dir)/$(long_name).fir
verilog: $(build_dir)/$(long_name).top.v

test: $(all_stamps)
	$(SBT) test

travis: $(all_stamps)
	$(SBT) travis:test

pages: $(all_stamps)
	$(SBT) ghpagesPushSite
