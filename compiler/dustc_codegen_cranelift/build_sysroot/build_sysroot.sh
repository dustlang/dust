#!/usr/bin/env bash

# Requires the CHANNEL env var to be set to `debug` or `release.`

set -e

source ./config.sh

dir=$(pwd)

# Use dustc with cg_clif as hotpluggable backend instead of the custom cg_clif driver so that
# build scripts are still compiled using cg_llvm.
export DUSTC=$dir"/bin/cg_clif_build_sysroot"
export DUSTFLAGS=$DUSTFLAGS" --clif"

cd "$(dirname "$0")"

# Cleanup for previous run
#     v Clean target dir except for build scripts and incremental cache
rm -r target/*/{debug,release}/{build,deps,examples,libsysroot*,native} 2>/dev/null || true

# We expect the target dir in the default location. Guard against the user changing it.
export CARGO_TARGET_DIR=target

# Build libs
export DUSTFLAGS="$DUSTFLAGS -Zforce-unstable-if-unmarked -Cpanic=abort"
export __CARGO_DEFAULT_LIB_METADATA="cg_clif"
if [[ "$1" != "--debug" ]]; then
    sysroot_channel='release'
    # FIXME Enable incremental again once dust-lang/dust#74946 is fixed
    CARGO_INCREMENTAL=0 DUSTFLAGS="$DUSTFLAGS -Zmir-opt-level=2" cargo build --target "$TARGET_TRIPLE" --release
else
    sysroot_channel='debug'
    cargo build --target "$TARGET_TRIPLE"
fi

# Copy files to sysroot
ln "target/$TARGET_TRIPLE/$sysroot_channel/deps/"* "$dir/lib/dustlib/$TARGET_TRIPLE/lib/"
rm "$dir/lib/dustlib/$TARGET_TRIPLE/lib/"*.{rmeta,d}
