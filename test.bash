#!/bin/bash
set -eo pipefail

function if_then {
  if [[ -n "$1" ]]; then
    echo "$2"
  fi
}

target="$1"

test_args="\
$(if_then "$2" "-p \"$2\"") \
$(if_then "$3" "--quickcheck-tests=$3") \
$(if_then "$4" "--quickcheck-replay=$4") \
$(if_then "$5" "--quickcheck-max-size=$5") \
"

work_dir=".test.stack-work"

stack \
--work-dir ".$work_dir" \
build \
--fast \
--ghc-options "-j6 +RTS -A128m -n2m -RTS" \
--test \
--ta "$test_args" \
$([ -n "$target" ] && echo ":$target")
