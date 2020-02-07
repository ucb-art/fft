#!/bin/bash
set -ex

git submodule update --init

cd dsp-framework
# Don't update riscv-tools since it's obnoxiously big.
git config --global submodule.riscv-tools.update none
git submodule update --init --recursive
git config --global --unset submodule.riscv-tools.update

./update.bash no_hwacha
cd ..
