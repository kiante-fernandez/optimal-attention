# Expected reward of the three response policies under a common parameter set, on random value sets
# drawn from the prior and on the empirical trials. Reports, per k: mean summed true value of the
# chosen set, mean sampling cost, net reward, and the fraction of the oracle value (top-k true values).
#   julia simulations/policy_cost.jl --sigma 2.6 --sample 0.0037 --switch 0.00995 --n 5000 [--fit results/fits/fit_simultaneous_stage2.csv]
using Distributions, Optim, StatsFuns, QuadGK, Memoize, Statistics, Random, Printf
const ROOT = dirname(dirname(@__FILE__))
include(joinpath(ROOT, "simulations/config.jl")); include(joinpath(ROOT, "src/utils.jl")); include(joinpath(ROOT, "src/meta_mdp.jl"))
include(joinpath(ROOT, "src/voi.jl")); include(joinpath(ROOT, "src/directed_cognition.jl")); include(joinpath(ROOT, "src/sequential.jl"))
using CSV; using DataFrames: DataFrame, nrow, eachrow
getarg(name, default) = (i = findfirst(==("--" * name), ARGS); isnothing(i) ? default : ARGS[i + 1])
σ = parse(Float64, getarg("sigma", "2.6")); γs = parse(Float64, getarg("sample", "0.0037")); γw = parse(Float64, getarg("switch", "0.00995"))
fit = getarg("fit", "")
if fit != ""
    d = CSV.read(joinpath(ROOT, fit), DataFrame); d = d[sortperm(d.loss), :]; σ, γs, γw = d.σ[1], d.γs[1], d.γw[1]
end
n = parse(Int, getarg("n", "5000")); β = 1000.0
println(@sprintf("parameters: σ=%.3f γ_sample=%.5f γ_switch=%.5f (%s)", σ, γs, γw, fit == "" ? "command line" : fit))

function cost_of(fix, γs, γw)
    isempty(fix) && return 0.0
    γs * length(fix) + γw * count(i -> fix[i] != fix[i-1], 2:length(fix))
end
function evaluate(values, label)
    println("== $label ==")
    println(rpad("policy", 14), " k   value   cost    net   oracle  %oracle  samples  P(first=best)  P(in order)")
    out = []
    for k in 1:3, pol in ["simultaneous", "sequential", "commit"]
        m = MetaMDP(; n_item=4, sub_size=k, σ_obs=σ, sample_cost=γs, switch_cost=γw)
        v = 0.0; c = 0.0; o = 0.0; s = 0.0; fib = 0.0; ino = 0.0
        for (i, u) in enumerate(values)
            Random.seed!(1000 + i)
            sim = simulate_policy(pol, m, State(u); β)
            v += sum(u[sim.choice]); c += cost_of(sim.fixations, γs, γw); o += sum(sort(u, rev=true)[1:k]); s += length(sim.fixations)
            rk = ranks(u); fib += rk[sim.choice[1]] == 1; ino += issorted(rk[sim.choice])
        end
        N = length(values)
        println(rpad(pol, 14), " ", k, @sprintf("  %6.3f  %6.3f  %6.3f  %6.3f   %5.1f    %5.1f     %5.2f        %5.2f", v/N, c/N, (v-c)/N, o/N, 100*(v-c)/o, s/N, fib/N, ino/N))
        push!(out, (; label, policy=pol, k, value=v/N, cost=c/N, net=(v-c)/N, oracle=o/N, samples=s/N, p_first_best=fib/N, p_in_order=ino/N))
    end
    out
end
Random.seed!(1)
res = evaluate([randn(4) for _ in 1:n], "values drawn from the prior N(0,1)")
vs = CSV.read(joinpath(ROOT, "results/value_sets.csv"), DataFrame; missingstring="NA")
emp = [[(r.item_value_0 - r.rating_mean) / r.rating_sd, (r.item_value_1 - r.rating_mean) / r.rating_sd, (r.item_value_2 - r.rating_mean) / r.rating_sd, (r.item_value_3 - r.rating_mean) / r.rating_sd] for r in eachrow(vs)]
res2 = evaluate(emp[1:min(n, length(emp))], "empirical value sets (standardized ratings)")
CSV.write(joinpath(ROOT, "results/tables/policy_cost_" * (fit == "" ? "callaway" : replace(basename(fit), ".csv" => "")) * ".csv"), DataFrame(vcat(res, res2)))
