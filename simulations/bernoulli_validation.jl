# Validation of the directed-cognition approximation in a Bernoulli version of subset selection, where
# the exact optimal policy can be computed by backward induction (as in Callaway et al., 2021, S1).
# Item i has an unknown success probability p_i ~ Beta(1, 1); a sample of item c is a Bernoulli(p_c)
# outcome; the belief is the vector of (successes + 1, failures + 1) counts; the terminal reward is the
# sum of the k highest posterior means; costs are as in the main model; at most T samples per trial.
#   julia simulations/bernoulli_validation.jl --T 12 --nsim 20000 --out results/tables/bernoulli_validation.csv
using Distributions, Random, Statistics, Printf
using CSV; using DataFrames: DataFrame
getarg(name, default) = (i = findfirst(==("--" * name), ARGS); isnothing(i) ? default : ARGS[i + 1])
T = parse(Int, getarg("T", "12")); NSIM = parse(Int, getarg("nsim", "20000")); outfile = getarg("out", "results/tables/bernoulli_validation.csv")
const ROOT = dirname(dirname(@__FILE__))

struct Prob; n::Int; k::Int; γs::Float64; γw::Float64; T::Int; end
pm(a, b) = a / (a + b)
means(α, β) = [pm(α[i], β[i]) for i in eachindex(α)]
term_reward(p, α, β) = sum(sort(means(α, β), rev=true)[1:p.k])
cost(p, last, c) = p.γs + ((last != 0 && last != c) ? p.γw : 0.0)
nsamples(α, β) = sum(α) + sum(β) - 2 * length(α)

# ---- exact optimal value, memoized backward induction (t is implied by the counts)
function Vstar!(cache, p, α, β, last)
    key = (α, β, last); haskey(cache, key) && return cache[key]
    v = term_reward(p, α, β)
    if nsamples(α, β) < p.T
        for c in 1:p.n
            q = pm(α[c], β[c])
            ev = -cost(p, last, c) + q * Vstar!(cache, p, Base.setindex(α, α[c] + 1, c), β, c) + (1 - q) * Vstar!(cache, p, α, Base.setindex(β, β[c] + 1, c), c)
            ev > v && (v = ev)
        end
    end
    cache[key] = v
end

# ---- directed cognition in the Bernoulli model: beta-binomial predictive of the successes in N samples
const PMF = Dict{Tuple{Int,Int,Int},Vector{Float64}}()
function bb_pmf(N, a, b)
    get!(PMF, (N, a, b)) do
        d = BetaBinomial(N, Float64(a), Float64(b)); [pdf(d, h) for h in 0:N]
    end
end
function voi_N(p, α, β, c, N, μ, ord)
    selected = c in view(ord, 1:p.k)
    cv = selected ? μ[ord[p.k + 1]] : μ[ord[p.k]]
    w = bb_pmf(N, α[c], β[c]); v = 0.0
    for h in 0:N
        m = (α[c] + h) / (α[c] + β[c] + N)
        v += w[h + 1] * (selected ? max(cv - m, 0.0) : max(m - cv, 0.0))
    end
    v
end
function voc_dc(p, α, β, last, c, Nmax, μ, ord)
    best = -Inf
    for N in 1:Nmax
        val = voi_N(p, α, β, c, N, μ, ord) - cost(p, last, c) - (N - 1) * p.γs
        val > best && (best = val)
    end
    best
end
# exact evaluation of the directed-cognition (simultaneous) policy
function Vdc!(cache, p, α, β, last)
    key = (α, β, last); haskey(cache, key) && return cache[key]
    v = term_reward(p, α, β)
    Nmax = p.T - nsamples(α, β)
    if Nmax > 0
        μ = means(α, β); ord = sortperm(μ, rev=true)
        vocs = [voc_dc(p, α, β, last, c, Nmax, μ, ord) for c in 1:p.n]
        best, c = findmax(vocs)
        if best > 0
            q = pm(α[c], β[c])
            v = -cost(p, last, c) + q * Vdc!(cache, p, Base.setindex(α, α[c] + 1, c), β, c) + (1 - q) * Vdc!(cache, p, α, Base.setindex(β, β[c] + 1, c), c)
        end
    end
    cache[key] = v
end

# ---- simulation of the three response policies (true values drawn from the prior)
function simulate(p, policy, ptrue; rng)
    α = ntuple(_ -> 1, p.n); β = ntuple(_ -> 1, p.n); last = 0; cst = 0.0; reward = 0.0
    remaining = collect(1:p.n); chosen = Int[]
    while length(chosen) < p.k
        krem = p.k - length(chosen)
        if length(remaining) == krem; append!(chosen, remaining); break; end
        Nmax = p.T - nsamples(α, β)
        sub = Prob(length(remaining), policy == :bestfirst ? 1 : krem, p.γs, p.γw, p.T)
        αs = ntuple(i -> α[remaining[i]], length(remaining)); βs = ntuple(i -> β[remaining[i]], length(remaining))
        f = findfirst(==(last), remaining); lasts = last == 0 ? 0 : (isnothing(f) ? -1 : f)
        μ = means(αs, βs); ord = sortperm(μ, rev=true)
        vocs = Nmax > 0 ? [voc_dc(sub, αs, βs, lasts, c, Nmax, μ, ord) for c in 1:sub.n] : fill(-Inf, sub.n)
        if policy == :commit
            sel = ord[1:krem]; lowest = sel[end]; unsel = ord[krem+1:end]
            safe = [i for i in sel if vocs[i] <= 0 && (i != lowest || maximum(vocs[unsel]) <= 0)]
            if !isempty(safe)
                for i in sort(safe, by = i -> -μ[i]); push!(chosen, remaining[i]); end
                deleteat!(remaining, sort(safe)); continue
            end
        end
        best, c = findmax(vocs)
        if best <= 0
            for i in ord[1:(policy == :bestfirst ? 1 : krem)]; push!(chosen, remaining[i]); end
            policy == :bestfirst ? deleteat!(remaining, ord[1]) : break
            continue
        end
        item = remaining[c]; cst += cost(p, last, item); last = item
        if rand(rng) < ptrue[item]; α = Base.setindex(α, α[item] + 1, item) else β = Base.setindex(β, β[item] + 1, item) end
    end
    sum(ptrue[chosen]) - cst
end
function sim_value(p, policy; nsim, seed=1)
    rng = MersenneTwister(seed); tot = 0.0
    for _ in 1:nsim; tot += simulate(p, policy, rand(rng, p.n); rng); end
    tot / nsim
end

rows = NamedTuple[]
println(@sprintf("%-3s %-6s %-6s %8s %8s %8s %8s %8s %8s", "k", "γs", "γw", "V*", "V_DC", "DC/V*", "sim_DC", "sim_BF", "sim_CWC"))
for k in 1:3, γs in (0.002, 0.01), γw in (0.0, 0.02)
    p = Prob(4, k, γs, γw, T)
    α0 = ntuple(_ -> 1, 4); β0 = ntuple(_ -> 1, 4)
    vs = Vstar!(Dict{Any,Float64}(), p, α0, β0, 0)
    vd = Vdc!(Dict{Any,Float64}(), p, α0, β0, 0)
    s_dc = sim_value(p, :simultaneous; nsim=NSIM); s_bf = sim_value(p, :bestfirst; nsim=NSIM); s_cw = sim_value(p, :commit; nsim=NSIM)
    oracle = k == 1 ? 0.8 : (k == 2 ? 0.8 + 0.6 : 0.8 + 0.6 + 0.4)   # E[sum of top-k of 4 uniforms]
    println(@sprintf("%-3d %-6.3f %-6.3f %8.4f %8.4f %8.3f %8.4f %8.4f %8.4f", k, γs, γw, vs, vd, vd / vs, s_dc, s_bf, s_cw))
    push!(rows, (; k, γs, γw, T, oracle, V_opt=vs, V_dc=vd, ratio_dc=vd/vs, sim_dc=s_dc, sim_bestfirst=s_bf, sim_commit=s_cw,
                   bestfirst_vs_opt=s_bf/vs, commit_vs_opt=s_cw/vs))
end
CSV.write(joinpath(ROOT, outfile), DataFrame(rows))
println("symmetry check (V*(k=3) - V*(k=1) should equal 1 at equal costs):")
for r in rows; r.k == 1 || continue; r3 = first(x for x in rows if x.k == 3 && x.γs == r.γs && x.γw == r.γw); println(@sprintf("  γs=%.3f γw=%.3f: %.4f", r.γs, r.γw, r3.V_opt - r.V_opt)); end
