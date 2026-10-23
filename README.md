# Fixation patterns in multi-response choice do not reflect optimal information sampling

Code and data for: Fernandez KA, Callaway F, Krajbich I. "Fixation patterns in multi-response choice do not reflect optimal information sampling" (manuscript for PLOS Computational Biology).

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
paper/plos/build.sh                  # PLOS PDF, S1 Appendix PDF, TIFF figures, and the Word working copy from paper/manuscript.md
python3 paper/build_chapter.py        # dissertation chapter version (APA 7 Word file) -> dissertation-papers/chapter-2/
```

## Project Structure

```
optimal-attention/
├── src/                      # Core model implementation
│   ├── meta_mdp.jl          # MetaMDP definition (State, Belief, Policy)
│   ├── voi.jl               # Value of information for subset selection (paper Eqs 9-10)
│   ├── directed_cognition.jl # Directed cognition approximation (paper Eq 8)
│   ├── sequential.jl        # Response policies: simultaneous, best-first, commit-when-certain
│   └── utils.jl             # Helper functions
├── simulations/             # Fitting and simulation
│   ├── run_empirical.jl     # Simulate a policy on the empirical trials
│   ├── fit_grid.jl          # Random-search parameter fit on summary statistics
│   ├── policy_cost.jl       # Expected reward of each policy (Table 3)
│   ├── bernoulli_validation.jl  # Directed cognition vs the exact optimum (S1 section G)
│   ├── run_fits.sh, run_best.sh, recovery.sh
│   └── config.jl            # Default parameters
├── analysis/                # Analysis and plotting
│   ├── build_data.R         # Data tables (positional ROI mapping, fixations in ms, phases)
│   ├── descriptives.R, tests.R, cluster_tests.R, figures.R, fig1_fig2.R
│   └── fit_table.R, policy_cost_table.R, make_recovery_targets.R, recovery_table.R
├── R/theme_pub.R            # Figure theme
├── data/                    # Empirical data (anonymous participant codes)
│   ├── choosekEYE_R.csv.gz  # Webcam gaze samples on item ROIs (from choose_k; gzipped)
│   └── choosek_R.csv        # Trial-level behavior (from choose_k)
├── results/                 # fits/ (parameter search), recovery/ (S1 section H), tables/
│                            # data_*.csv, value_sets.csv, model_*.csv are build products and not tracked
├── figures/paper/           # Figs 1-9 (PNG; TIFFs are built into paper/plos/)
└── paper/                   # manuscript.md (source), plos/ (PLOS build, S1 Appendix, submission fields),
                             # build_chapter.py (dissertation chapter), STATUS_AND_PLAN.md
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
- **Value of Information** (`src/voi.jl`): Computes the expected value of acquiring additional information about each item (paper Eqs 9-10)
- **Directed Cognition** (`src/directed_cognition.jl`): Implements the DC approximation policy for tractable attention allocation (paper Eq 8)

## Empirical Data

`data/choosek_R.csv` holds trial-level behavior (ratings-based item values, choices, cumulative response times) and `data/choosekEYE_R.csv.gz` holds the webcam gaze samples (gzipped; read_csv reads it directly) assigned to item regions of interest (50 px buffer; `button_ROI` codes j, i, l, k are the left, top, right, bottom items). Both come from the `choose_k` pipeline. Participants are identified by the anonymous codes P01-P78 used in that repository's raw files; P05 is excluded (condition-assignment bug), and trials with four selections are dropped. `analysis/build_data.R` turns the two files into the tables in `results/` (`data_trials.csv`, `data_fixations.csv`, `value_sets.csv`, `data_summary_by_k.csv`, `data_subject_ratings.csv`); `results/tables/gaze_quality_by_subject.csv` gives each participant's share of raw samples that fell on an item. The data files are tracked in this repository (participants carry anonymous codes). The derived tables are build products and are not tracked; `build_data.R` regenerates them in about 20 seconds.
