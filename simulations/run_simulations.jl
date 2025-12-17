using Distributed
using Printf
using CSV
using DataFrames, DataFramesMeta

include("config.jl")
include("../src/utils.jl")
include("../src/meta_mdp.jl")
include("../src/voi.jl")
include("../src/directed_cognition.jl")

# %% ==================== load functions ====================

# simulate function is now in meta_mdp.jl (returns fixations field)

function generate_values(n_item::Int = 3, mean_value::Float64 = 0)
    #Here we just want to generate some set of normally distributed
    # values for a given number of items
    d = Normal(mean_value,1) #arbitray
    rand(d, n_item)
end
# %% ==================== set up simulation parameters ====================

# use relative paths so everyone can run the code
DIR = "../results/res_subset_size_"
N_SIMS = DEFAULT_N_SIMS  # from config.jl

# %% ==================== run simulation  ====================

# It's a suuuuper useful utility function, similar to the expand.grid function in R
function grid(;kws...)
    X = map(Iterators.product(values(kws)...)) do x
        (; zip(keys(kws), x)...)
    end
end


function run_simulation(n_item; n_sims=N_SIMS)
    to_sim = grid(
        trial_idx = 1:n_sims,
        # avg_value_idx = 0:.2:2,  # 2 is a very strong bias
        avg_value_idx = 0.0,      # start with Normal(0,1)
        sub_size = 1:n_item-1,
    )[:]  # [:] flattens the matrix
    map(to_sim) do (;trial_idx, sub_size, avg_value_idx)
        m = MetaMDP(;n_item, sub_size,
            σ_obs=DEFAULT_σ_OBS,
            sample_cost=DEFAULT_SAMPLE_COST,
            switch_cost=DEFAULT_SWITCH_COST)
        dc = DirectedCognition(m; β=DC_BETA)
        vals = generate_values(n_item, avg_value_idx)
        sim = simulate(dc, State(vals))            
        (;sub_size, trial_idx, vals, sim...)
    end
end

all_sims = run_simulation(4; n_sims=1000);

# %% ==================== trials.csv - one row per trial ====================

trials = @chain DataFrame(all_sims) begin
    @rtransform :choice = join(:choice, ",")
    @rtransform :vals = join(:vals, ",")
    @rtransform :attended = join(:fixations, ",")  # rename fixations→attended for output
    DataFrames.select(Not(:fixations))  # drop the original fixations column
end
CSV.write("../results/trials.csv", trials)

# %% ==================== fixations.csv - one row per fixations ====================

function parse_fixations(attended)
    targets = Int[]; durations = Int[]
    current = -1
    for f in attended
        if f != current
            current = f
            push!(targets, f)
            push!(durations, 1)
        else
            durations[end] += 1
        end
    end
    (targets, durations)
end

function make_fixations_frame(sims)
    flatmap(sims) do sim
        (;choice, fixations, vals) = sim
        targets, durations = parse_fixations(fixations)
        mean_val = mean(vals)
        val_ranks = ranks(vals)
        mean_choice_val = mean(vals[choice])
        seen = Set{Int}()
        map(eachindex(targets), targets, durations) do fix_num, fixated, duration
            push!(seen, fixated)
            mean_seen_val = mean(vals[s] for s in seen)
            fixated_val = vals[fixated]
            is_chosen = fixated in choice
            (;sim.sub_size, sim.trial_idx, fix_num, fixated, duration, fixated_val, 
              fixated_rank=val_ranks[fixated], is_chosen, mean_val, mean_seen_val)
        end
    end |> DataFrame
end

fixations = make_fixations_frame(all_sims)
CSV.write("../results/fixations.csv", fixations)
