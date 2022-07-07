using Memoize
using Distributions
using Random
using QuadGK
using StatsFuns: normcdf, normpdf

"Value of information from n samples of item c"
#need to sort μ and c!! (convert into and index in the sorted space)
function voi_n(m::MetaMDP, b::Belief, c::Computation, n)
    rank = sortperm(b.μ)
    c_sort = findfirst(isequal(c), rank)
    mu_sort = b.µ[rank]
    cv = try
        competing_value(mu_sort, c_sort, m.sub_size)
    catch
        println(rank, b.μ, c, " ", c_sort)
        rethrow()
    end
    σ_μ = std_of_posterior_mean(b.λ[c], m.σ_obs / √n)
    σ_μ ≈ 0. && return 0.  # avoid error initializing Normal
    d = Normal(b.µ[c], σ_μ)
    expect_max_dist(d, cv) - comparison_value(mu_sort, c_sort, m.sub_size)
end

"Myopic value of information"
voi1(m, b, c) = voi_n(m, b, c, 1)

"Value of perfect information about one action"
function voi_action(m::MetaMDP, b::Belief, a::Int)
    cv = competing_value(b.µ, a)
    d = Normal(b.µ[a], b.λ[a] ^ -0.5)
    expect_max_dist(d, cv) - maximum(b.µ)
end

"Value of perfect information about all items"
function vpi(m::MetaMDP, b::Belief)
    expected_max_norm(b.μ, b.λ) - maximum(b.μ)
end


# ==================== helpers ====================

"First value on the other side of the subset barrier
NOTE: μ must be sorted
"
function competing_value(µ::Vector{Float64}, a::Int, k::Int)
    n = length(μ)
    if a ≤ n - k
        μ[n-k+1]
    else
        μ[n-k]
    end
end

"First value on the other side of the subset barrier
NOTE: μ must be sorted
"
function comparison_value(µ::Vector{Float64}, a::Int, k::Int)
    n = length(μ)
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

"Standard deviation of the posterior mean"

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
