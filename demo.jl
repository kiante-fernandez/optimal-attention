using Distributed
using Printf

include("utils.jl")
include("meta_mdp.jl")
include("voi.jl")
include("directed_cognition.jl")
include("bmps.jl")
include("bmps_bayesopt.jl")

m = MetaMDP(n_item=5,sub_size =2, σ_obs=2.6, sample_cost=.003, switch_cost=.01)

# %% ==================== simulate trials ====================

function simulate(pol::Policy, s::State)
    fixations = Int[]
    roll = rollout(pol; s) do b, c
        if c != 0
            push!(fixations, c)
        end
    end
    (;roll.choice, fixations)
end

mg = MetaGreedy(m)
s = State(m)
sim_res = simulate(mg, s)
sim_res.choice
# %% ==================== compare policies ====================


function evaluate(pol::Policy; name=string(typeof(pol)))
    @time reward, steps = monte_carlo(10000) do
        sim = rollout(pol)
        [sim.reward, sim.steps-1]
    end
    @printf "%-20s reward: %.3f  steps: %.3f" name reward steps
end

evaluate(mg)
simulate(mg, State([-.2, .3, .4]))

dc = DirectedCognition(m; β=1000)
evaluate(dc)
simulate(dc, State([-.2, .3, .4]))

bmps1 = BMPSPolicy(m, [0.01, .97, .00, .03])
evaluate(bmps1)
simulate(bmps1, State([-.2, .3, .4]))

# %% --------
bmps = optimize_bmps(m, n_iter=100, verbose=true)
evaluate(bmps)

# %% --------
