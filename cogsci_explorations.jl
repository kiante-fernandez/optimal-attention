using Distributed
using Printf
using RCall
R"""
source("base.r")
"""
using DataFrames

include("utils.jl")
include("meta_mdp.jl")
include("voi.jl")
include("directed_cognition.jl")
# include("bmps.jl")
# include("bmps_bayesopt.jl")

# %% --------

m = MetaMDP(n_item=6,sub_size =3, σ_obs=2.6, sample_cost=.0037, switch_cost=.0995)

b = Belief(m)
µs = -3:.01:3

df = flatmap(1:5) do k
    m1 = mutate(m; sub_size=k)
    v = map(µs) do µ
        voc = monte_carlo(1000) do
            b.µ .= randn(6)
            b.µ[1] = µ
            voi_n(m1, b, 1, 1)
        end
        (;
            k = k,
            mean_value = µ,
            voc = voc,
        )
    end
end |> DataFrame
@rput df

R"""
df %>%
    ggplot(aes(mean_value, voc, color=factor(k))) +
    geom_line()

fig()
"""

# %% --------

function parse_fixations(attended)
    targets = Int[]; durations = Int[]
    current = -1
    for f in attended
        if f != current
            current = f
            push!(targets, f)
            push!(durations, 1)
        else
            durations[end] += 1
        end
    end
    (targets, durations)
end


dt = 10
true_values = -3:.5:3


# m = MetaMDP(n_item=6,sub_size =3, σ_obs=2.6 * √dt, sample_cost=.0037 / dt, switch_cost=.03)
m = MetaMDP(n_item=6,sub_size =3, σ_obs=2.6, sample_cost=.0037, switch_cost=.0995)

df = flatmap(1:5) do k
    m1 = mutate(m; sub_size=k)
    dc = DirectedCognition(m1; β=1000)
    s = State(m)

    v = map(true_values) do true_value
        first_fix_duration = monte_carlo(1000) do
            s.value .= randn(6)
            s.value[1] = true_value
            sim = simulate(dc, s; max_steps=10)[2]
            targets, durations = parse_fixations(sim)
            durations[1]
        end
        (;
            k,
            true_value,
            first_fix_duration,
        )
    end
end |> DataFrame
@rput df

R"""
df %>%
    ggplot(aes(true_value, first_fix_duration, color=factor(k))) +
    geom_line()

fig()
"""