
"""
Adapted from https://github.com/JuliaMath/Roots.jl/
"""
function find_positive(
    f,
    xs;
    maxevals=100000,
)
    if length(xs) == 1 # secant needs x0, x1; only x0 given
        a = float(xs[1])

        h = eps(one(real(a)))^(1 / 3)
        da = h * oneunit(a) + abs(a) * h^2 # adjust for if eps(a) > h
        b = a + da
    else
        a, b = promote(float(xs[1]), float(xs[2]))
    end
    secant_positiv(f, a, b, maxevals)
end

function find_positive(f, a::T, b::T, maxevals) where {T}
    fa, fb = f(a), f(b)
    fa == fb && return false

    for i in 1:maxevals
        m = b - (b - a) * fb / (fb - fa)
        fm = f(m)
        fm > 0 && return true

        iszero(fm) && return true
        isnan(fm) && return false
        abs(fm) <= adjustunit * max(uatol, abs(m) * rtol) && return true
        if fm == fb
            sign(fm) * sign(f(nextfloat(m))) <= 0 && return true
            sign(fm) * sign(f(prevfloat(m))) <= 0 && return true
            return false
        end
        a, b, fa, fb = b, m, fb, fm
    end
    return false
end