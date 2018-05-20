base_dir ?= $(abspath .)
lib_dir = $(base_dir)/lib
framework_dir = $(base_dir)/dsp-framework
ivy_dir = $(base_dir)/.ivy2
ROCKETCHIP_DIR=$(framework_dir)/rocket-chip
TESTCHIPIP_DIR=$(framework_dir)/testchipip

include $(base_dir)/Makefrag
