using Memoize
using Random
using Distributions
using StatsBase
using Serialization
# using OnlineStats

BMPSWeights = NamedTuple{(:cost, :voi1, :voi_action, :vpi),Tuple{Float64,Float64,Float64,Float64}}
"A metalevel policy that uses the BMPS features"
struct BMPSPolicy <: Policy
    m::MetaMDP
    θ::BMPSWeights
    β::Float64
end
# BMPSPolicy(m, θ; kws...) = BMPSPolicy(;m, θ, kws...)
BMPSPolicy(m::MetaMDP, θ::Vector{Float64}, α=Inf) = BMPSPolicy(m, BMPSWeights(θ), float(α))


"VOC without VPI feature"
function fast_voc(pol::BMPSPolicy, b::Belief)
    (;m, θ) = pol
    map(1:m.n_item) do c
        -cost(pol.m, b, c) +
        -θ.cost +
        θ.voi1 * voi1(m, b, c) +
        θ.voi_action * voi_action(m, b, c)
    end
end

"Full VOC"
function voc(pol::BMPSPolicy, b::Belief)
    fast_voc(pol, b) .+ pol.θ.vpi * vpi(pol.m, b)
end

"Selects a computation to take in the given belief state"
function select(pol::BMPSPolicy, b::Belief; clever=true)
    (;m, θ) = pol
    voc = fast_voc(pol, b)

    if !clever  # computationally inefficient, but clearly correct
        voc .+= θ.vpi * vpi(m, b)
        if pol.β == Inf
            v, c = findmax(voc)
            return (v > 0) ? c : ⊥
        else
            p = softmax(pol.β .* [0; voc])
            return sample(0:m.n_item, Weights(p))
        end
    end

    if pol.β < Inf
        # gumbel-max trick
        voc .+= rand(Gumbel(), m.n_item) ./ pol.β
        voc .-= rand(Gumbel()) / pol.β  # for term action
    else
        # break ties randomly
        voc .+= 1e-10 * rand(length(voc))
    end

    # Choose candidate based on cheap voc
    v, c = findmax(voc)

    # Try putting VPI weight on VOI_action (a lower bound on VPI)
    v + θ.vpi * voi_action(m, b, c) > 0 && return c

    θ.vpi == 0. && return ⊥  # no weight on VPI, VOC can't improve

    # Try actual VPI.
    v + θ.vpi * vpi(m, b) > 0 && return c

    # Nope.
    return ⊥
end


# ---------- Optimization helpers ---------- #

"Identifies the cost parameter that makes a hard-maximizing policy never take any computations."
function max_cost(m::MetaMDP)
    θ = [1., 0, 0, 1]
    b = Belief(m)
    # s = State(m)
    # b = Belief(s)
    function computes()
        pol = BMPSPolicy(m, θ)
        all(select(pol, b) != ⊥ for i in 1:30)
    end

    while computes()
        θ[1] *= 2
    end

    while !computes()
        θ[1] /= 2
        if θ[1] < 2^-10
            error("Computation is too expensive")
        end
    end

    step_size = θ[1] / 10
    while computes()
        θ[1] += step_size
    end
    θ[1]
end

"Transforms a value from the 3D unit hybercube to weights for BMPS"
function x2theta(mc, x)
    # This is a trick to go from two Uniform(0,1) samples to 
    # a unifomrm sample in the 3D simplex.
    voi_weights = diff([0; sort(collect(x[2:3])); 1])
    [x[1] * mc; voi_weights]
end



 