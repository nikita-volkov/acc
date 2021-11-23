#!/bin/bash
set -eo pipefail

./format.bash
./bench.bash length/cons/10000
