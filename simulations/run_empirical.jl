# Simulate a policy on the empirical trial value sets.
#   julia -p 8 simulations/run_empirical.jl --policy sequential --sigma 2.6 --sample 0.0037 --switch 0.00995 \
#         --beta 1000 --alpha 1 --nrep 10 --seed 1 --out results/model_sequential
# Writes <out>_trials.csv and <out>_fixations.csv. Items are 0-based to match the data.
using Distributed
@everywhere begin
    using Distributions, Optim, StatsFuns, QuadGK, Memoize, Statistics, Random
    const ROOT = dirname(dirname(@__FILE__))
    include(joinpath(ROOT, "simulations/config.jl"))
    include(joinpath(ROOT, "src/utils.jl"))
    include(joinpath(ROOT, "src/meta_mdp.jl"))
    include(joinpath(ROOT, "src/voi.jl"))
    include(joinpath(ROOT, "src/directed_cognition.jl"))
    include(joinpath(ROOT, "src/sequential.jl"))
end
using CSV; using DataFrames: DataFrame, nrow, eachrow

function getarg(name, default)
    i = findfirst(==("--" * name), ARGS)
    isnothing(i) ? default : ARGS[i + 1]
end
policy  = getarg("policy", "sequential")
σ_obs   = parse(Float64, getarg("sigma", "2.6"))
γ_samp  = parse(Float64, getarg("sample", "0.0037"))
γ_swi   = parse(Float64, getarg("switch", "0.00995"))
β       = parse(Float64, getarg("beta", "1000"))
α       = parse(Float64, getarg("alpha", "1"))
n_rep   = parse(Int, getarg("nrep", "10"))
seed    = parse(Int, getarg("seed", "1"))
out     = getarg("out", "results/model_" * policy)
subset  = getarg("subset", "all")        # all | even | odd (trial number parity)
fit     = getarg("fit", "")              # a fit results file: use its best (lowest-loss) parameters
if fit != ""
    d = CSV.read(joinpath(ROOT, fit), DataFrame); d = d[sortperm(d.loss), :]
    σ_obs, γ_samp, γ_swi = d.σ[1], d.γs[1], d.γw[1]
    println("parameters from $fit: σ=$σ_obs γ_sample=$γ_samp γ_switch=$γ_swi")
end

vs = CSV.read(joinpath(ROOT, "results/value_sets.csv"), DataFrame; missingstring = "NA")
subset == "even" && (vs = vs[iseven.(vs.trial), :])
subset == "odd"  && (vs = vs[isodd.(vs.trial), :])

@everywhere function run_trial(policy, row, k, σ_obs, γ_samp, γ_swi, β, α, n_rep, seed)
    v = [row.item_value_0, row.item_value_1, row.item_value_2, row.item_value_3]
    u = (v .- α * row.rating_mean) ./ row.rating_sd          # standardized values (Callaway et al. Eq 8)
    m = MetaMDP(; n_item=4, sub_size=k, σ_obs, sample_cost=γ_samp, switch_cost=γ_swi)
    Random.seed!(hash((seed, row.subject_id, row.trial)))
    trials = NamedTuple[]; fixes = NamedTuple[]
    for rep in 1:n_rep
        sim = simulate_policy(policy, m, State(u); β)
        targets, durs = Int[], Int[]
        for f in sim.fixations
            if isempty(targets) || targets[end] != f; push!(targets, f); push!(durs, 1) else durs[end] += 1 end
        end
        onsets = cumsum(vcat(0, durs))[1:end-1]
        rk = ranks(u)
        top = Set(sortperm(u, rev=true)[1:k])
        ch = sim.choice
        push!(trials, (; row.subject_id, row.trial, k, rep,
            choice_1 = ch[1] - 1, choice_2 = k >= 2 ? ch[2] - 1 : missing, choice_3 = k >= 3 ? ch[3] - 1 : missing,
            resp_1 = sim.response_steps[1], resp_2 = k >= 2 ? sim.response_steps[2] : missing,
            resp_3 = k >= 3 ? sim.response_steps[3] : missing,
            n_fix = length(targets), total_samples = sum(durs), correct = Set(ch) == top,
            first_is_best = rk[ch[1]] == 1))
        for (i, (t, d, o)) in enumerate(zip(targets, durs, onsets))
            phase = something(findfirst(r -> o < r, sim.response_steps), k + 1)
            chosen_at = findfirst(==(t), ch)
            push!(fixes, (; row.subject_id, row.trial, k, rep, fix_num = i, n_fix = length(targets),
                item = t - 1, onset = o, duration = d, value_z = u[t], rank = rk[t],
                chosen_at = isnothing(chosen_at) ? missing : chosen_at, phase))
        end
    end
    trials, fixes
end

t0 = time()
res = pmap(eachrow(vs)) do row
    run_trial(policy, row, Int(row.k), σ_obs, γ_samp, γ_swi, β, α, n_rep, seed)
end
trials = DataFrame(reduce(vcat, first.(res)))
fixes  = DataFrame(reduce(vcat, last.(res)))
CSV.write(joinpath(ROOT, out * "_trials.csv"), trials)
CSV.write(joinpath(ROOT, out * "_fixations.csv"), fixes)
println("policy=$policy σ=$σ_obs γs=$γ_samp γw=$γ_swi β=$β α=$α nrep=$n_rep: ",
        nrow(trials), " trials, ", nrow(fixes), " fixations in ", round(time() - t0, digits=1), " s")
for k in 1:3
    tk = trials[trials.k .== k, :]
    println("  k=$k: mean fix ", round(mean(tk.n_fix), digits=2), ", mean samples ", round(mean(tk.total_samples), digits=1),
            ", accuracy ", round(mean(tk.correct), digits=3), ", first is best ", round(mean(tk.first_is_best), digits=3))
end
