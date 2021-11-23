#!/bin/bash
set -eo pipefail

./format.bash
./bench.bash 10000
