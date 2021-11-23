#!/bin/bash
set -eo pipefail

./format.bash
./bench.bash append
