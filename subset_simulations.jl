using Distributed
using Printf
using CSV
using DataFrames

include("utils.jl")
include("meta_mdp.jl")
include("voi.jl")
include("directed_cognition.jl")
include("bmps.jl")
include("bmps_bayesopt.jl")

# %% ==================== load functions ====================

function simulate(pol::Policy, s::State)
    fixations = Int[]
    roll = rollout(pol; s) do b, c
        if c != 0
            push!(fixations, c)
        end
    end
    (;roll.choice, fixations)
end

# FRED: this should probably be "generate_values"
function generate_states(n_item::Int = 3, mean_value::Float64 = 2)
    #Here we just want to generate some set of normally distributed
    # values for a given number of items
    d = Normal(mean_value,1) #arbitray
    rand(d, n_item)
end
# %% ==================== set up simulation parameters ====================

# FRED: use relative paths so everyone can run the code
dir = "simulation_results/res_subset_size_"
n_sims = 10

# %% ==================== run simulation  ====================

# FRED: it's fine if this looks like magic to you
# It's a suuuuper useful utility function, similar to the expand.grid function in R
function grid(;kws...)
    X = map(Iterators.product(values(kws)...)) do x
        (; zip(keys(kws), x)...)
    end
end

function write_simulation(n_item; dir=dir, n_sims=n_sims)
    for subset_idx in 1:n_item - 1
        m = MetaMDP(;n_item, sub_size = subset_idx, σ_obs=2.6, sample_cost=.003, switch_cost=.01)
        dc = DirectedCognition(m; β=1000)

        to_sim = grid(
            avg_value_idx = 0:.2:2,  # 2 is a very strong bias
            trial_idx = 1:n_sims
        )[:]  # [:] flattens the matrix
        trials = map(to_sim) do (;avg_value_idx, trial_idx) # note: with ; the name matters and order doesn't
            ss = generate_states(n_item, avg_value_idx) #generate some values
            sim = simulate(dc, State(ss))
            (;avg_value_idx, trial_idx, ss, sim...)
        end
        file_name = string(dir, subset_idx,".csv")
        CSV.write(file_name, DataFrame(trials))
    end
end

write_simulation(6)
