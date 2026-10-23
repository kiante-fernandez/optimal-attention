# Random search over (σ_obs, γ_sample, γ_switch) for one policy, scored on summary statistics
# of the even-numbered trials against results/data_summary_by_k.csv.
#   julia -p 8 simulations/fit_grid.jl --policy sequential --n 200 --seed 1 --out results/fit_sequential.csv
# Loss: squared deviations of per-k statistics (mean fixations, mean total fixation time in ms at
# 100 ms per sample, accuracy; and for k>1 P(first response is the best item), P(responses in rank
# order), and fixation time before the first response), each divided by a scale (1 fixation, 500 ms,
# 0.05 for probabilities). Dwell shares are reported, not fitted.
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
    const MS_PER_SAMPLE = 100.0
end
using CSV; using DataFrames: DataFrame, nrow, eachrow

getarg(name, default) = (i = findfirst(==("--" * name), ARGS); isnothing(i) ? default : ARGS[i + 1])
policy = getarg("policy", "sequential"); n_cfg = parse(Int, getarg("n", "200")); seed = parse(Int, getarg("seed", "1"))
β = parse(Float64, getarg("beta", "1000")); α = parse(Float64, getarg("alpha", "1"))
out = getarg("out", "results/fit_" * policy * ".csv")
ranges = Dict(:σ => (1.0, 6.0), :γs => (0.0005, 0.03), :γw => (0.001, 0.3))   # log-uniform
refine = getarg("refine", "")   # a stage-1 results file: search a box around its 10 best configurations
if refine != ""
    d = CSV.read(joinpath(ROOT, refine), DataFrame); d = d[sortperm(d.loss), :][1:min(10, nrow(d)), :]
    box(x) = (minimum(x) / 1.3, maximum(x) * 1.3)
    ranges = Dict(:σ => box(d.σ), :γs => box(d.γs), :γw => box(d.γw))
    println("refining in ", ranges)
end

vs = CSV.read(joinpath(ROOT, "results/value_sets.csv"), DataFrame; missingstring = "NA")
vs = vs[iseven.(vs.trial) .& vs.has_gaze, :]
target_file = getarg("target", "results/data_summary_by_k.csv")   # summary statistics to fit (data, or a simulated set for recovery)
target = CSV.read(joinpath(ROOT, target_file), DataFrame; missingstring = "NA")
vsrows = [(; subject_id=r.subject_id, trial=r.trial, k=Int(r.k), v=[r.item_value_0, r.item_value_1, r.item_value_2, r.item_value_3],
            mean=r.rating_mean, sd=r.rating_sd) for r in eachrow(vs)]

@everywhere function sim_stats(policy, rows, σ, γs, γw, β, α, seed)
    acc = zeros(3); nfix = zeros(3); tsum = zeros(3); share = zeros(3, 4); fib = zeros(3); n = zeros(3); t1 = zeros(3); inord = zeros(3)
    for r in rows
        u = (r.v .- α * r.mean) ./ r.sd
        m = MetaMDP(; n_item=4, sub_size=r.k, σ_obs=σ, sample_cost=γs, switch_cost=γw)
        Random.seed!(hash((seed, r.subject_id, r.trial)))
        sim = simulate_policy(policy, m, State(u); β)
        k = r.k; n[k] += 1
        acc[k] += Set(sim.choice) == Set(sortperm(u, rev=true)[1:k])
        f = sim.fixations
        nf = isempty(f) ? 0 : 1 + count(i -> f[i] != f[i-1], 2:length(f))
        nfix[k] += nf; tsum[k] += length(f) * MS_PER_SAMPLE
        if !isempty(f)
            rk = ranks(u); cnt = zeros(4)
            for x in f; cnt[rk[x]] += 1; end
            share[k, :] .+= cnt ./ length(f)
        end
        rk = ranks(u)
        fib[k] += rk[sim.choice[1]] == 1
        t1[k] += sim.response_steps[1] * MS_PER_SAMPLE
        inord[k] += issorted(rk[sim.choice])
    end
    (; acc = acc ./ n, nfix = nfix ./ n, time = tsum ./ n, share = share ./ n, fib = fib ./ n, t1 = t1 ./ n, inord = inord ./ n)
end

function loss(st, target)
    L = 0.0
    for k in 1:3
        t = target[target.k .== k, :][1, :]
        L += ((st.nfix[k] - t.mean_n_fix) / 1.0)^2 + ((st.time[k] - t.mean_total_fix_time) / 500.0)^2 +
             ((st.acc[k] - t.accuracy) / 0.05)^2
        # dwell shares are not fitted: webcam gaze misassignment attenuates them (see Methods); they are compared as predictions
        if k >= 2
            L += ((st.fib[k] - t.p_first_resp_is_best) / 0.05)^2 + ((st.inord[k] - t.p_in_rank_order) / 0.05)^2 +
                 ((st.t1[k] - t.mean_fix_time_before_resp1) / 500.0)^2
        end
    end
    L
end

Random.seed!(seed)
logu(a, b) = exp(rand(Uniform(log(a), log(b))))
cfgs = [(σ = logu(ranges[:σ]...), γs = logu(ranges[:γs]...), γw = logu(ranges[:γw]...)) for _ in 1:n_cfg]
t0 = time()
stats = pmap(cfgs) do c
    sim_stats(policy, vsrows, c.σ, c.γs, c.γw, β, α, seed)
end
rows = [(; policy, cfg.σ, cfg.γs, cfg.γw, β, α, loss = loss(st, target),
          acc1 = st.acc[1], acc2 = st.acc[2], acc3 = st.acc[3], nfix1 = st.nfix[1], nfix2 = st.nfix[2], nfix3 = st.nfix[3],
          time1 = st.time[1], time2 = st.time[2], time3 = st.time[3],
          share1_r1 = st.share[1, 1], share1_r4 = st.share[1, 4], share3_r1 = st.share[3, 1], share3_r4 = st.share[3, 4],
          fib2 = st.fib[2], fib3 = st.fib[3], inord2 = st.inord[2], inord3 = st.inord[3], t1_2 = st.t1[2], t1_3 = st.t1[3]) for (cfg, st) in zip(cfgs, stats)]
df = sort(DataFrame(rows), :loss)
CSV.write(joinpath(ROOT, out), df)
println("$(n_cfg) configs in $(round(time() - t0, digits=1)) s; best:")
show(first(df, 8), allcols=true); println()
