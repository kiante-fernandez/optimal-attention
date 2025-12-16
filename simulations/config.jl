# Configuration constants for optimal attention simulations
# Centralized parameters to avoid magic numbers throughout the codebase

# Observation and cost parameters (from empirical fitting)
const DEFAULT_σ_OBS = 2.6           # Standard deviation of observation noise
const DEFAULT_SAMPLE_COST = 0.0037  # Cost per sample
const DEFAULT_SWITCH_COST = 0.0995  # Cost for switching attention to different item

# Policy parameters
const DC_BETA = 1000.0              # Inverse temperature for Directed Cognition softmax
const DC_MAX_SAMPLES = 100          # Maximum samples to consider in DC optimization
const DC_ABS_TOL = 1.0              # Absolute tolerance for sample count optimization

# Simulation parameters
const DEFAULT_N_SIMS = 10000        # Default number of simulations
