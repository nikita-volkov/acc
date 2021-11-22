#!/bin/bash
set -eo pipefail

function if_then {
  if [[ -n "$1" ]]; then
    echo "$2"
  fi
}

pattern=$1

ba="-s"
if [[ -n "$pattern" ]]
then
  ba="$ba -m pattern \"$pattern\""
fi

stack \
--work-dir .bench.stack-work \
build \
--ghc-options "-O2 -threaded" \
--bench \
--ba "$ba" \
acc:bench
