foo(x, y; z) = (x,y,z)
foo(1,2,3)

foo(1,2;z=3, ω)


z = 3
foo(1,2; z)

z0 = 3
foo(1,2; z0)

foo(z, z; z)


foo(x, y; z, ω) = (x,y, z, ω)
foo(1, 2; ω=3, z)

# how do do
map(x->2*x+1, 1:5)
map(1:5) do x
    2x + 1
end
