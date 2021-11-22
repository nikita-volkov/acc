#!/bin/bash
set -eo pipefail

function if_then {
  if [[ -n "$1" ]]; then
    echo "$2"
  fi
}

work_dir=".typecheck.stack-work"

stack \
--work-dir "$work_dir" \
build \
--fast \
--ghc-options "-threaded -j6 +RTS -A128m -n2m -RTS" \
--bench \
--no-run-benchmarks \
--test \
--no-run-tests
