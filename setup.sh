#!/bin/bash
set -ex

PATCHES_DIR="$PWD/patches"

git submodule update --init

cd dsp-framework
# Don't update riscv-tools since it's obnoxiously big.
git config --global submodule.riscv-tools.update none
git submodule update --init --recursive || true
git config --global --unset submodule.riscv-tools.update

# Apply patches to hack over versioning hell
pushd barstools
git reset --hard HEAD
git apply $PATCHES_DIR/barstools.patch
pushd mdf
git reset --hard HEAD
git apply $PATCHES_DIR/mdf.patch
popd
popd

pushd chisel-testers
git reset --hard HEAD
git apply ../../patches/chisel-testers.patch
popd

pushd dsptools
git reset --hard HEAD
git apply ../../patches/dsptools.patch
popd

./update.bash no_hwacha
cd ..
