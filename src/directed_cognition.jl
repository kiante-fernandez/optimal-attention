using Optim
using StatsFuns: normcdf, normpdf
using Distributions

# Note: config.jl should be included before this file for constants

"""
Directed Cognition (roughly) as defined by Gabaix and Laibson (2005).
(Paper Section 2.3: Directed Cognition Approximation)

The DC policy does a limited kind of look-ahead by considering different
amounts of additional sampling that it could commit to. It estimates the VOC
for taking one additional sample to be max_N VOC(take N samples). This is a
lower bound on the true VOC.
"""

"""
    voc_n(m, b, c, n)

The value of computation from taking `n` samples of item `c`.
(Paper Eq. 8: VOC_N(b,c) = VOI_N(b,c) - cost)

Returns VOI minus total sampling cost (including any switch cost).
"""
function voc_n(m, b, c, n)
    total_cost = cost(m, b, c) + m.sample_cost * (n-1)  # may include switch cost
    voi_n(m, b, c, n) - total_cost
end

@kwdef struct DirectedCognition <: SoftmaxPolicy
    m::MetaMDP
    β::Float64 = 1e10
end
DirectedCognition(m; kws...) = DirectedCognition(;m, kws...)

"""
Directed Cognition approximation to the value of computation.
Optimizes over the number of samples to find the best commitment.
"""
function voc(pol::DirectedCognition, b, c)
    # Treat number of samples as continuous and optimize
    # Use constants from config.jl if available, otherwise defaults
    max_samples = @isdefined(DC_MAX_SAMPLES) ? DC_MAX_SAMPLES : 100
    abs_tol = @isdefined(DC_ABS_TOL) ? DC_ABS_TOL : 1.0
    res = optimize(1, max_samples, GoldenSection(), abs_tol=abs_tol) do n
        -voc_n(pol.m, b, c, n)
    end
    -res.minimum
end



