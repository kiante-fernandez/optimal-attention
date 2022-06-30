using Distributed
using Printf

include("utils.jl")
include("meta_mdp.jl")
include("voi.jl")
include("directed_cognition.jl")
include("bmps.jl")
include("bmps_bayesopt.jl")

m = MetaMDP(σ_obs=2.6, sample_cost=.003)

# %% ==================== Compare policies ====================

function evaluate(pol::Policy; name=string(typeof(pol)))
    @time reward, steps = monte_carlo(10000) do
        sim = rollout(pol)
        [sim.reward, sim.steps-1]
    end
    @printf "%-20s reward: %.3f  steps: %.3f" name reward steps
end


mg = MetaGreedy(m)
evaluate(mg)

dc = DirectedCognition(m; β=1000)
evaluate(dc)

bmps1 = BMPSPolicy(m, [0.005, .95, .001, .05])  
evaluate(bmps1)

# %% --------
bmps = optimize_bmps(m, n_iter=100, verbose=true)
evaluate(bmps)
