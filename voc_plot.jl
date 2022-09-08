using Distributed
using Printf
using CSV
using DataFrames
using Plots

include("utils.jl")
include("meta_mdp.jl")
include("voi.jl")
include("directed_cognition.jl")


d = Normal(0,1)
d = Uniform(-2,2)
n = 4
rand(d, n)
values = rand(d, n)
mu =1
[mu; values]

function get_voc(mu, k, n)
    #b = Belief([mu, -2,-1,0,1,2], ones(n), 0)
    d = Uniform(-2,2)
    values = rand(d, n)
    b = Belief([mu; values], ones(n), 0)
    m = MetaMDP(n_item= length(b.μ),sub_size = k, σ_obs=2.6, sample_cost=.0037, switch_cost=.0995)
    voc_n(m,b,1,1)

end 



get_voc(1, 1)


#now make a version of this function that samples the other values randomly
#think about fred MC function in utils
#you will have 

function monte_carlo(f, N=10000)
    mean(1:N) do i
        f()
    end
end



#plot
x = -3:0.1:3
plot(x, get_voc.(x, 1))

