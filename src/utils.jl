using Statistics

# like map, but concatenates results (which are lists)
# use for nested maps
flatmap(f, lists...) = mapreduce(f, vcat, lists...)

# ranks items in x, 1 is maximal
ranks(x) = sortperm(sortperm(x; rev=true))
