using Printf
using Parameters
using SplitApplyCombine
using Serialization

function SplitApplyCombine.invert(d::AbstractArray{<:AbstractDict{K, V}}) where {K, V}
    result = Dict(k => [v] for (k, v) in pairs(d[1]))
    for i in 2:length(d)
        for (k, v) in pairs(d[i])
            push!(result[k], v)
        end
    end
    result
end

function describe_vec(x::Vector)
    @printf("%.3f ± %.3f  [%.3f, %.3f]\n", juxt(mean, std, minimum, maximum)(x)...)
end

juxt(fs...) = (xs...) -> Tuple(f(xs...) for f in fs)
Base.map(f, d::AbstractDict) = [f(k, v) for (k, v) in d]
valmap(f, d::AbstractDict) = Dict(k => f(v) for (k, v) in d)
valmap(f) = d->valmap(f, d)
keymap(f, d::AbstractDict) = Dict(f(k) => v for (k, v) in d)
juxt(fs...) = x -> Tuple(f(x) for f in fs)
repeatedly(f, n) = [f() for i in 1:n]

dictkeys(d::AbstractDict) = (collect(keys(d))...,)
dictvalues(d::AbstractDict) = (collect(values(d))...,)
namedtuple(d::AbstractDict) = NamedTuple{dictkeys(d)}(dictvalues(d))

function softmax(x)
    ex = exp.(x .- maximum(x))
    ex ./= sum(ex)
    ex
end

Base.dropdims(idx::Int...) = X -> dropdims(X, dims=idx)
Base.reshape(idx::Union{Int,Colon}...) = x -> reshape(x, idx...)

Serialization.serialize(path::String) = x->serialize(path, x)

function cache(f, file; disable=false, read_only=false)
    disable && return f()
    isfile(file) && return deserialize(file)
    read_only && error("No cached result $file")
    result = f()
    serialize(file, result)
    result
end

function mutate(x::T; kws...) where T
    for field in keys(kws)
        if !(field in fieldnames(T))
            error("$(T.name) has no field $field")
        end
    end
    return T([get(kws, fn, getfield(x, fn)) for fn in fieldnames(T)]...)
end

function dropnames(namedtuple::NamedTuple, names...)
    keepnames = Base.diff_names(Base._nt_names(namedtuple), names)
   return NamedTuple{keepnames}(namedtuple)
end

getfields(x) = (getfield(x, f) for f in fieldnames(typeof(x)))

# type2dict(x::T) where T = Dict(fn=>getfield(x, fn) for fn in fieldnames(T))

function background(f, name; save=false)
    @async begin
        try
            x, t = @timed f()
            println(name, " finished in ", round(t), " seconds")
            mkpath("background_tasks")
            serialize("background_tasks/$name", x)
            return x
        catch e
            println("ERROR: $name failed")
            rethrow(e)
        end
    end
end

macro timeout(seconds, expr)
    quote
        tsk = @task $expr
        schedule(tsk)
        Timer($seconds) do timer
            istaskdone(tsk) || Base.throwto(tsk, InterruptException())
        end
        fetch(tsk)
    end
end

function timeout(f, seconds)
    tsk = Task(f)
    schedule(tsk)
    Timer(seconds) do timer
        istaskdone(tsk) || Base.throwto(tsk, InterruptException())
    end
    fetch(tsk)
end

function monte_carlo(f, N=10000)
    mean(1:N) do i
        f()
    end
end

function repeatedly(f, N=10000)
    map(1:N) do i
        f()
    end
end

round1(x) = round(x; digits=1)
round2(x) = round(x; digits=2)
round3(x) = round(x; digits=3)
