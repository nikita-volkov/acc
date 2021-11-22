#!/bin/bash
set -eo pipefail

stack \
--work-dir .bench.stack-work \
build \
--ghc-options "-O2 -threaded" \
--bench \
acc:bench
