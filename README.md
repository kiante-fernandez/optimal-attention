# Optimal Information Sampling in Multi-Response Settings

Code for: Fernandez & Callaway. "Optimal Information Sampling in Multi-Response Settings."

## Overview

This repository contains simulation and analysis code for studying optimal attention allocation in multi-response problems. The model uses a metalevel Markov Decision Process (MDP) framework to determine how decision-makers should allocate attention when selecting k items from n alternatives.

## Reproducing Paper Results

### Requirements

**Julia 1.9+** with packages:
- Distributions, Optim, StatsFuns, QuadGK
- CSV, DataFrames, DataFramesMeta
- Memoize, StatsBase, Parameters, SplitApplyCombine

**R 4.0+** with packages:
- tidyverse, ggplot2, here, readr, dplyr, magrittr

### Running Simulations

```bash
cd simulations
julia run_simulations.jl
```

This generates:
- `results/trials.csv` - One row per trial with choice and attention data
- `results/fixations.csv` - One row per fixation with timing and value information

## Project Structure

```
optimal-attention/
├── src/                      # Core model implementation
│   ├── meta_mdp.jl          # MetaMDP definition (State, Belief, Policy)
│   ├── voi.jl               # Value of Information calculations (Eq. 5-7)
│   ├── directed_cognition.jl # Directed Cognition approximation (Eq. 8)
│   └── utils.jl             # Helper functions
├── simulations/             # Simulation scripts
│   ├── run_simulations.jl   # Main simulation script
│   └── config.jl            # Model parameters
├── analysis/                # Analysis and plotting
│   ├── plotting.R           # Model-only figures
│   └── model_data_comparison.R  # Model vs. data comparison
├── data/                    # Empirical data
│   └── choosekEYE_R.csv     # Eye-tracking data from behavioral experiment
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

The `data/choosekEYE_R.csv` file contains eye-tracking data from a behavioral experiment where participants selected k items from 4 alternatives. The `model_data_comparison.R` script:
- Filters for correct trials (participants selected the k highest-valued items)
- Compares model predictions to empirical fixation patterns
- Generates figures showing both individual subject variability and aggregate trends
