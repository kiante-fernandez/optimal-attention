using Distributed
using Printf
using CSV

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

function generate_states(n_item::Int = 3, mean_value::Int = 2)
    #Here we just want to generate some set of normally distributed
    # values for a given number of items
    d = Normal(mean_value,1) #arbitray
    rand(d, n_item)
end
# %% ==================== set up simulation parameters ====================

dir = "/Users/kiantefernandez/Documents/Julia/optimal-attention/simulation_results/res_subset_size_"
n_sims = 10
choice_vector = []

# %% ==================== run simulation  ====================
for subset_idx in 1:n_item - 1
    choice_vector = []
    for avg_value_idx in 1:10
        ss = generate_states(n_item, avg_value_idx) #generate some values
        for trial_idx in 1:n_sims
            m = MetaMDP(n_item=6,sub_size = subset_idx, σ_obs=2.6, sample_cost=.003, switch_cost=.01)
            dc = DirectedCognition(m; β=1000)
            #push!(choice_vector, [trial_idx, ss, simulate(dc, State(ss))])
            push!(choice_vector, simulate(dc, State(ss)))

        end
    end
    file_name = string(dir,subset_idx,".csv")
    #CSV.write(file_name,choice_vector, header=["trial", "values", "choice", "fixations"])
    CSV.write(file_name,choice_vector)
end
