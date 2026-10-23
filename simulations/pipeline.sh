#!/bin/bash
# Full model pipeline: fits -> simulations at the best parameters -> figures.
set -e
cd "$(dirname "$0")/.."
simulations/run_fits.sh
NREP=5 simulations/run_best.sh
Rscript analysis/figures.R > logs/figures.log 2>&1
Rscript analysis/fig1_fig2.R > logs/fig1_fig2.log 2>&1
echo PIPELINE_DONE
