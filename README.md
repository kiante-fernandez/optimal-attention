# Optimal Information Sampling in Multi-Response Settings

Code for: Fernandez, Callaway, & Krajbich. "Optimal Information Sampling in Multi-Response Settings."

## Overview

This repository contains simulation and analysis code for studying optimal attention allocation in multi-response problems. The model uses a metalevel Markov Decision Process (MDP) framework to determine how decision-makers should allocate attention when selecting k items from n alternatives.

## Reproducing Paper Results

The pipeline, from the repository root:

```bash
Rscript analysis/build_data.R        # trial, fixation, and value-set tables from data/ -> results/
Rscript analysis/descriptives.R      # Table 1 (results/tables/descriptives_by_k.csv)
Rscript analysis/tests.R             # mixed-effects tests (results/tables/tests.txt)
Rscript analysis/cluster_tests.R     # cluster-based permutation tests of the time-resolved index -> results/tables/cluster_tests.txt
simulations/run_fits.sh              # fit sigma, gamma_sample, gamma_switch per policy (results/fits/)
NREP=5 simulations/run_best.sh       # simulate each policy on the empirical trials at its best parameters
Rscript analysis/figures.R           # Figs 3-9 (figures/paper/), data vs policies, odd-numbered trials
Rscript analysis/fig1_fig2.R         # Fig 1 (task, belief updating) and Fig 2 (VOC maps)
julia simulations/policy_cost.jl --fit results/fits/fit_simultaneous_stage2.csv   # cost of each response policy
julia simulations/bernoulli_validation.jl   # directed cognition vs the exact optimum (Bernoulli model), S1 section G
Rscript analysis/make_recovery_targets.R && simulations/recovery.sh && Rscript analysis/recovery_table.R   # S1 section H
paper/plos/build.sh                  # PLOS PDF, TIFF figures, Word and PDF working copies from paper/manuscript.md
```

`simulations/pipeline.sh` runs the fits, the best-parameter simulations, and the figure scripts in sequence.

## Project Structure

```
optimal-attention/
├── src/                      # Core model implementation
│   ├── meta_mdp.jl          # MetaMDP definition (State, Belief, Policy)
│   ├── voi.jl               # Value of information for subset selection (paper Eqs 6-7)
│   ├── directed_cognition.jl # Directed Cognition approximation (paper Eq 5)
│   ├── sequential.jl        # Response policies: simultaneous, best-first, commit-when-certain
│   └── utils.jl             # Helper functions
├── simulations/             # Simulation scripts
│   ├── run_simulations.jl   # Model-only simulation on random value sets
│   ├── run_empirical.jl     # Simulate a policy on the empirical trials
│   ├── fit_grid.jl          # Random-search parameter fit on summary statistics
│   ├── policy_cost.jl       # Expected reward of each policy
│   ├── run_fits.sh, run_best.sh, pipeline.sh
│   └── config.jl            # Default parameters
├── analysis/                # Analysis and plotting
│   ├── build_data.R         # Data tables (positional ROI mapping, fixations in ms, phases)
│   ├── descriptives.R, tests.R, figures.R, fig1_fig2.R
│   └── model_data_comparison.R  # Earlier two-figure comparison (superseded by figures.R)
├── data/                    # Empirical data
│   ├── choosekEYE_R.csv.gz  # Webcam gaze samples on item ROIs (from choose_k; gzipped)
│   └── choosek_R.csv        # Trial-level behavior (from choose_k)
├── paper/                   # manuscript.md (source), plos/ (PLOS build), STATUS_AND_PLAN.md
├── results/                 # Simulation output
│   ├── trials.csv
│   └── fixations.csv
├── figures/                 # Generated figures
└── archive/                 # Development artifacts
```

## Model Parameters

Default parameters (from `simulations/config.jl`):

| Parameter | Value | Description |
|-----------|-------|-------------|
| n | 4 | Number of items |
| k | 1-3 | Subset size (items to select) |
| σ_obs | 2.6 | Observation noise |
| γ_sample | 0.0037 | Cost per sample |
| γ_switch | 0.0995 | Cost to switch attention |

## Core Components

- **MetaMDP** (`src/meta_mdp.jl`): Defines the metalevel decision problem including state representation, belief updates, and simulation
- **Value of Information** (`src/voi.jl`): Computes the expected value of acquiring additional information about each item (Paper Eq. 5-7)
- **Directed Cognition** (`src/directed_cognition.jl`): Implements the DC approximation policy for tractable attention allocation (Paper Eq. 8)

## Empirical Data

`data/choosek_R.csv` holds trial-level behavior (ratings-based item values, choices, cumulative response times) and `data/choosekEYE_R.csv.gz` holds the webcam gaze samples (gzipped; read_csv reads it directly) assigned to item regions of interest (50 px buffer; `button_ROI` codes j, i, l, k are the left, top, right, bottom items). Both come from the `choose_k` pipeline. Participants are identified by the anonymous codes P01-P78 used in that repository's raw files; P05 is excluded (condition-assignment bug), and trials with four selections are dropped. `analysis/build_data.R` turns the two files into the tables in `results/` (`data_trials.csv`, `data_fixations.csv`, `value_sets.csv`, `data_summary_by_k.csv`, `data_subject_ratings.csv`); `results/tables/gaze_quality_by_subject.csv` gives each participant's share of raw samples that fell on an item. The data files are tracked in this repository (participants carry anonymous codes). `analysis/model_data_comparison.R` and `analysis/quantitative_fit_metrics.R` are the earlier correct-trials-only comparison and are kept for reference.
