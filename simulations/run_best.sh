#!/bin/bash
# Simulate each policy on every usable empirical trial at its best-fitting parameters (stage 2 search).
set -eo pipefail
cd "$(dirname "$0")/.."
NREP=${NREP:-5}
for pol in commit simultaneous sequential; do
  f=results/fits/fit_${pol}_stage2.csv; [ -f "$f" ] || f=results/fits/fit_${pol}_stage1.csv
  echo "$pol: simulating at the best parameters of $f ($(date))"
  julia -p 8 simulations/run_empirical.jl --policy $pol --fit $f --beta 1000 --alpha 1 --nrep $NREP --seed 11 --out results/model_$pol > logs/run_best_$pol.log 2>&1
  grep -vE "WARNING|^\s*$" logs/run_best_$pol.log | tail -4
done
echo RUN_BEST_DONE
