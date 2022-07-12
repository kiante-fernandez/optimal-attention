# Code and data for: "Uncovering the optimal information sampling policy underlying subset choice"

## Important files

- `meta_mdp.jl` defines the metalevel Markov decision process.
- `voi.jl` defines the value of information features (need to be changed if used)
- `bmps.jl` defines the policy, based on the VOI features.
- `bmps_ucb.jl` defines the UCB method for identifying near-optimal settings of the BMPS weights.
- `directed_cognition.jl` defines


## TODO

- ~~Generate some sets of States with different characteristics for simulating some choice, rt, and fixation data.~~
- Create plots for the following: Probability of choosing the first-seen item as a function of the first-fixation duration & first fixation duration as a function of the rating of the first-fixated item. Best off making these plots in R
- Clean up code and review code. Retain only code that is needed todo the simulations. Otherwise, you will confuse yourself about what gets used where when.
