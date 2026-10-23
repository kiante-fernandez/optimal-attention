"""
Sequential policy. Solve "choose one of the remaining items" with the current beliefs using
the Directed Cognition policy, commit the item with the highest posterior mean, remove it,
and repeat until `m.sub_size` items have been chosen. Beliefs carry over between stages, and
the switching cost applies across stages (the item fixated last is remembered; if it has been
committed, the next sample is a switch).

Returns the chosen items in response order, the fixation sequence over the whole trial (full
item indices), and `response_steps[r]`, the number of samples taken before response r.
"""
function simulate_sequential(m::MetaMDP, s::State; β=1e3, max_steps=1000)
    n, k = m.n_item, m.sub_size
    b = Belief(s)
    remaining = collect(1:n)
    chosen = Int[]; fixations = Int[]; response_steps = Int[]
    for r in 1:k
        if length(remaining) == k - r + 1          # everything left must be taken
            append!(chosen, remaining)
            append!(response_steps, fill(length(fixations), length(remaining)))
            break
        end
        m_sub = MetaMDP(; n_item=length(remaining), sub_size=1, σ_obs=m.σ_obs,
                          sample_cost=m.sample_cost, switch_cost=m.switch_cost)
        pol = DirectedCognition(m_sub; β)
        steps = 0
        while true
            f = findfirst(==(b.focused), remaining)
            focused_sub = b.focused == 0 ? 0 : (isnothing(f) ? -1 : f)   # -1: last item was committed, so sampling is a switch
            b_sub = Belief(b.μ[remaining], b.λ[remaining], focused_sub)
            c_sub = steps >= max_steps ? ⊥ : select(pol, b_sub)
            if c_sub == ⊥
                best = remaining[argmax(noisy(b.μ[remaining]))]
                push!(chosen, best); push!(response_steps, length(fixations))
                deleteat!(remaining, findfirst(==(best), remaining))
                break
            end
            c = remaining[c_sub]
            push!(fixations, c)
            transition!(m, b, s, c)
            steps += 1
        end
    end
    (; choice=chosen, fixations, response_steps, belief=b)
end

"Simultaneous policy with the same return structure: all responses are made when sampling stops, highest mean first."
function simulate_simultaneous(m::MetaMDP, s::State; β=1e3, max_steps=1000)
    pol = DirectedCognition(m; β)
    sim = simulate(pol, s; max_steps)
    (; choice=collect(sim.choice), fixations=sim.fixations,
       response_steps=fill(length(sim.fixations), m.sub_size), belief=nothing)
end

"""
Commit-when-certain policy (option-level stopping). The agent runs the Directed Cognition
policy on the subset problem that remains, but commits an item as soon as no computation with
positive value could still change that item's membership in the chosen set (Hay et al., 2012,
blinkered decomposition, applied to each response). For a currently selected item i this means
(a) sampling i itself is not worth it (its estimate is unlikely to fall below the best
unselected item), and (b) if i is the lowest selected item, sampling any unselected item is not
worth it either, because only the lowest selected item can be displaced. Committed items leave
the problem, which shrinks to (n - committed, k - committed). When several items are safe at
once they are committed highest mean first. `hold_cost` >= 0 is an optional cost of holding an
unexecuted decision; with 0 the rule commits when the VOC is non-positive. For k = 1 the policy
coincides with the simultaneous policy.
"""
function simulate_commit(m::MetaMDP, s::State; β=1e3, max_steps=1000, hold_cost=0.0)
    n, k = m.n_item, m.sub_size
    b = Belief(s)
    remaining = collect(1:n)
    chosen = Int[]; fixations = Int[]; response_steps = Int[]
    steps = 0
    while length(chosen) < k
        k_rem = k - length(chosen); n_rem = length(remaining)
        if n_rem == k_rem
            append!(chosen, remaining); append!(response_steps, fill(length(fixations), n_rem))
            break
        end
        m_sub = MetaMDP(; n_item=n_rem, sub_size=k_rem, σ_obs=m.σ_obs, sample_cost=m.sample_cost, switch_cost=m.switch_cost)
        pol = DirectedCognition(m_sub; β)
        f = findfirst(==(b.focused), remaining)
        b_sub = Belief(b.μ[remaining], b.λ[remaining], b.focused == 0 ? 0 : (isnothing(f) ? -1 : f))
        v = [voc(pol, b_sub, c) for c in 1:n_rem]
        order = sortperm(b_sub.μ, rev=true)
        selected = order[1:k_rem]; unselected = order[k_rem+1:end]
        lowest_sel = selected[end]
        max_unsel = maximum(v[unselected])
        safe = [i for i in selected if v[i] <= -hold_cost && (i != lowest_sel || max_unsel <= -hold_cost)]
        if !isempty(safe)
            for i in sort(safe; by = i -> -b_sub.μ[i])
                push!(chosen, remaining[i]); push!(response_steps, length(fixations))
            end
            deleteat!(remaining, sort(safe))
            continue
        end
        c_sub = steps >= max_steps ? ⊥ : select(pol, b_sub)
        if c_sub == ⊥
            for i in selected
                push!(chosen, remaining[i]); push!(response_steps, length(fixations))
            end
            break
        end
        push!(fixations, remaining[c_sub]); transition!(m, b, s, remaining[c_sub]); steps += 1
    end
    (; choice=chosen, fixations, response_steps, belief=b)
end

"Dispatch on the policy name used by the simulation scripts."
function simulate_policy(policy::AbstractString, m::MetaMDP, s::State; β=1e3, max_steps=1000)
    policy == "simultaneous" ? simulate_simultaneous(m, s; β, max_steps) :
    policy == "sequential"   ? simulate_sequential(m, s; β, max_steps) :
    policy == "commit"       ? simulate_commit(m, s; β, max_steps) :
    error("unknown policy $policy")
end
