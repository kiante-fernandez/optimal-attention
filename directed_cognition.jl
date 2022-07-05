using Optim
using StatsFuns: normcdf, normpdf
using Distributions

"Directed Cognition (roughly) as defined by Gabaix and Laibson (2005).

The DC policy does a limited kind of look-ahead by considering different
amounts of additional sampling that it could commit to. It estimates the VOC
for taking one additional sample to be max_N VOC(take N samples). This is a
lower bound on the true VOC.
"

function voc_n(m, b, c, n)
    total_cost = cost(m, b, c) + m.sample_cost * (n-1)  # may include switch cost
    voi_n(m, b, c, n) - total_cost
end

@kwdef struct DirectedCognition <: SoftmaxPolicy
    m::MetaMDP
    β::Float64 = 1e10
end
DirectedCognition(m; kws...) = DirectedCognition(;m, kws...)

"Directed Cognition approximation to the value of computation."
function voc(pol::DirectedCognition, b, c)
    # note that we treat the number of samples as a continuous variable here
    # and we assume you can't take more than 100
    res = optimize(1, 100, GoldenSection(), abs_tol=1.) do n  # note abs_tol is on the number of samples
        -voc_n(pol.m, b, c, n)
    end
    voc = -res.minimum
    voc
end


@kwdef struct MetaGreedy <: SoftmaxPolicy
    m::MetaMDP
    n::Int = 1
    β::Float64 = 1e10
end
MetaGreedy(m; kws...) = MetaGreedy(;m, kws...)
voc(pol::MetaGreedy, b, c) = voc_n(pol.m, b, c, 1)
