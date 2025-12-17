using Memoize
using Distributions
using Random
using QuadGK
using StatsFuns: normcdf, normpdf

"""
    voi_n(m::MetaMDP, b::Belief, c::Computation, n)

Value of information from taking `n` samples of item `c`.
(Paper Eq. 7: VOI_N(b,c))

Computes the expected improvement in terminal reward from sampling item `c`
exactly `n` times, compared to making a decision immediately.

# Arguments
- `m`: The metalevel MDP containing problem parameters
- `b`: Current belief state (μ values will be sorted internally)
- `c`: Index of the item to sample (1 to n_item)
- `n`: Number of samples to take

# Returns
Expected improvement in selection quality (can be negative if sampling is not worthwhile).
"""
function voi_n(m::MetaMDP, b::Belief, c::Computation, n)
    rank = sortperm(b.μ)
    c_sort = findfirst(isequal(c), rank)
    mu_sort = b.μ[rank]
    cv = try
        competing_value(mu_sort, c_sort, m.sub_size)
    catch
        println(rank, b.μ, c, " ", c_sort)
        rethrow()
    end
    σ_μ = std_of_posterior_mean(b.λ[c], m.σ_obs / √n)
    σ_μ ≈ 0. && return 0.  # avoid error initializing Normal
    d = Normal(b.μ[c], σ_μ)
    expect_max_dist(d, cv) - comparison_value(mu_sort, c_sort, m.sub_size)
end




# ==================== helpers ====================

"""
    competing_value(μ::Vector{Float64}, a::Int, k::Int)

First value on the other side of the subset barrier.
(Paper Eq. 6: competing value cv(c))

Returns the threshold value that item `a` must exceed to change selection.

# Arguments
- `μ`: Mean values vector (must be sorted in ascending order)
- `a`: Index of the item in the sorted array (1 to n)
- `k`: Subset size (number of items to select, must be 1 ≤ k < n)
"""
function competing_value(μ::Vector{Float64}, a::Int, k::Int)
    n = length(μ)
    @assert 1 ≤ k < n "k must be between 1 and n-1 (got k=$k, n=$n)"
    @assert 1 ≤ a ≤ n "a must be between 1 and n (got a=$a, n=$n)"
    if a ≤ n - k
        μ[n-k+1]
    else
        μ[n-k]
    end
end

"""
    comparison_value(μ::Vector{Float64}, a::Int, k::Int)

Comparison value for computing expected improvement.
Similar to competing_value but returns the item's own value when in top-k.

# Arguments
- `μ`: Mean values vector (must be sorted in ascending order)
- `a`: Index of the item in the sorted array (1 to n)
- `k`: Subset size (number of items to select, must be 1 ≤ k < n)
"""
function comparison_value(μ::Vector{Float64}, a::Int, k::Int)
    n = length(μ)
    @assert 1 ≤ k < n "k must be between 1 and n-1 (got k=$k, n=$n)"
    @assert 1 ≤ a ≤ n "a must be between 1 and n (got a=$a, n=$n)"
    if a ≤ n - k
        μ[n-k+1]
    else
        μ[a]
    end
end

"Expected maximum of a distribution and a constant"
function expect_max_dist(d::Distribution, constant::Float64)
    p_improve = 1 - cdf(d, constant)
    p_improve < 1e-10 && return constant
    (1 - p_improve) * constant + p_improve * mean(Truncated(d, constant, Inf))
end

"""
    std_of_posterior_mean(λ, σ_obs)

Standard deviation of where the posterior mean will land after one observation.
(Paper Eq. 5: σ_μ̃)

Given a prior with precision `λ` and observation noise `σ_obs`, computes
the standard deviation of the posterior mean (before observing).

# Mathematical derivation
- Prior: μ ~ N(μ₀, 1/λ)
- Observation: x ~ N(μ, σ_obs²)
- Posterior mean: μ̃ = w·x + (1-w)·μ₀ where w = λ_obs/(λ + λ_obs)
- Var(μ̃) = w²·(σ_obs² + 1/λ)
"""
function std_of_posterior_mean(λ, σ_obs)
    obs_λ = σ_obs ^ -2
    w = obs_λ / (λ + obs_λ)
    sample_sigma = √(1/λ + 1/obs_λ)
    w * sample_sigma
end

"Expected maximum of Normals with means μ and precisions λ"
function expected_max_norm(μ, λ)
    if length(μ) == 2
        μ1, μ2 = μ
        σ1, σ2 = λ .^ -0.5
        θ = √(σ1^2 + σ2^2)
        return μ1 * normcdf((μ1 - μ2) / θ) + μ2 * normcdf((μ2 - μ1) / θ) + θ * normpdf((μ1 - μ2) / θ)
    end

    dists = Normal.(μ, λ.^-0.5)
    mcdf(x) = mapreduce(*, dists) do d
        cdf(d, x)
    end

    - quadgk(mcdf, -10, 0, atol=1e-5)[1] + quadgk(x->1-mcdf(x), 0, 10, atol=1e-5)[1]
end
