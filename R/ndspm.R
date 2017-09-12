# simulate the rolling of an nds die (e.g. 2d6) plus a modifier
# name comes from:           ---                -      -

ndspm <- function(n, sides, mod = 0)
{
    sum(sample(1:sides, size = n, replace = TRUE)) + mod
}
