#!/bin/bash
set -eo pipefail

./format.bash
./bench.bash fromList/10000
