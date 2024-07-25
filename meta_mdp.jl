using Base: @kwdef
@isdefined(⊥) || const ⊥ = 0  # terminal action
@isdefined(Computation) || const Computation = Int

noisy(x, ε=1e-10) = x .+ ε .* rand(length(x))

"Metalevel Markov decision process"
@kwdef struct MetaMDP
    n_item::Int = 3                # number of items to choose between
    sub_size::Int = 2              # number of items to select
    σ_obs::Float64 = 1             # std of observation distribution
    sample_cost::Float64 = 0.001   # cost per sample
    switch_cost::Float64 = 0.      # additional cost for sampling a different item
end


"Ground truth state"
struct State
    value::Vector{Float64}  # values of each item in the choice set
end

State(m::MetaMDP) = State(randn(m.n_item))
#note that below you could write this out as (same thing):
#function State(m::MetaMDP)
#    State(randn(m.n_item))
#end


"Belief state"
mutable struct Belief
    µ::Vector{Float64}  # mean vector
    λ::Vector{Float64}  # precision vector
    focused::Int        # currently fixated item (necessary for switch cost)
#    ranks::Vector{Int}
end
#note two below are the same, we can simplify:
function Belief(n_item::Int)
    Belief(zeros(n_item), ones(n_item), 0 )
end
Belief(s::State) = Belief( length(s.value))
Belief(m::MetaMDP) = Belief( m.n_item)
#this above is quite common

Base.copy(b::Belief) = Belief(copy(b.µ), copy(b.λ), b.focused)


"Base type for metalevel policies."
abstract type Policy end

struct RandomPolicy <: Policy
    m::MetaMDP
end
select(pol::RandomPolicy, b::Belief) = rand(0:pol.m.n_item)

abstract type SoftmaxPolicy <: Policy end

function select(pol::SoftmaxPolicy, b)
    #we doing the softmax, but efficently
    noise = Gumbel(0, 1/pol.β)
    v, c = findmax(eachindex(b.μ)) do c
        voc(pol, b, c) + rand(noise)
    end
#    v0cs = map(c->voc(pol, b, c), eachindex(b.μ))
#    probs = softmax([0; vocs])
#    sample(0:4, Weights(probs)
    v > rand(noise) ? c : ⊥
end

"""Expected reward for making a decision.

We use the expected reward rather than the ground truth value because it has the same
expectation but lower variance. This makes learning more efficient, but dosen't
introduce any bias or change the optimal policy. See https://arxiv.org/abs/1408.2048
"""
function term_reward(m::MetaMDP,b::Belief)
    #maximum(b.µ)
    # note we now need to change for subset
    sum(partialsort(b.µ, 1:m.sub_size, rev = true))
end

"Sampling cost function, includes switching cost."
function cost(m::MetaMDP, b::Belief, c::Computation)
    if b.focused != 0 && b.focused != c
        return m.sample_cost + m.switch_cost
    else
        return m.sample_cost
    end
end

"Updates belief based on the given computation."
function transition!(m, b::Belief, s::State, c::Computation)
    b.focused = c
    # obs = s.value[c] + randn() * m.σ_obs  # SAME AS BELOW
    obs = rand(Normal(s.value[c], m.σ_obs))
    b.µ[c], b.λ[c] = bayes_update_normal(b.μ[c], b.λ[c], obs, m.σ_obs ^ -2)
end

"Returns updated mean and precision given a prior and observation."
function bayes_update_normal(μ, λ, obs, λ_obs)
    λ1 = λ + λ_obs
    μ1 = (obs * λ_obs + μ * λ) / λ1
    (μ1, λ1)
end

"Run one rollout (one decision) of a policy on its associated MetaMDP."
function rollout(policy::Policy; s=State(policy.m), max_steps=1000, logger=(b, c)->nothing)
    m = policy.m
    b = Belief(s)
    reward = 0
    for t in 1:max_steps
        c = (t == max_steps) ? ⊥ : select(policy, b) #? is ifelse in R
        logger(b, c)
        if c == ⊥
            reward += term_reward(m,b)
            choice = partialsortperm(noisy(b.µ), 1:m.sub_size, rev = true)
            return (;reward, choice, steps=t, state=s, belief=b)
        else
            reward -= cost(m, b, c)
            transition!(m, b, s, c)
        end
    end
end

# for do block syntax
rollout(logger::Function, policy; kws...) = rollout(policy; kws..., logger=logger)

function simulate(pol::Policy, s::State; max_steps=1000)
    fixations = Int[]
    roll = rollout(pol; s, max_steps) do b, c
        if c != 0
            push!(fixations, c)
        end
    end
    (;roll.choice, fixations)
end