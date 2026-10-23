#!/bin/bash
# Fit all three policies: random search (200 configs), then 150 configs in a box around the 10 best.
# Resumes from existing stage-1 files.
set -eo pipefail
cd "$(dirname "$0")/.."
for pol in commit simultaneous sequential; do
  s1=results/fits/fit_${pol}_stage1.csv
  if [ ! -f "$s1" ]; then
    echo "$pol stage 1 start: $(date)"
    julia -p 8 simulations/fit_grid.jl --policy $pol --n 200 --seed 1 --out $s1 > logs/fit_${pol}_stage1.log 2>&1
  fi
  echo "$pol stage 2 start: $(date)"
  julia -p 8 simulations/fit_grid.jl --policy $pol --n 150 --seed 2 --refine $s1 --out results/fits/fit_${pol}_stage2.csv > logs/fit_${pol}_stage2.log 2>&1
  echo "$pol done: $(date)"
done
echo ALL_FITS_DONE
