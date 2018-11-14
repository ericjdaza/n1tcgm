### N1T.CGM randomization.


## Set fixed seed.
scalar.seed <- 1707312341
scalar.seed.eric <- 1708290039


## Randomize Kate: TRUE = AB, FALSE = BA.

# 170801t.
set.seed(scalar.seed + 1)
( runif(1) > 0.5 ) # BA

# 170807m.
set.seed(scalar.seed + 170807)
( runif(1) > 0.5 ) # AB

# 170813u.
set.seed(scalar.seed + 170813)
( runif(1) > 0.5 ) # AB

# 170819s.
set.seed(scalar.seed + 170819)
( runif(1) > 0.5 ) # AB


## Randomize Eric J.: TRUE = AB, FALSE = BA.

# 170828m.
set.seed(scalar.seed.eric + 170828)
( runif(1) > 0.5 ) # BA

# 170903u.
set.seed(scalar.seed.eric + 170903)
( runif(1) > 0.5 ) # AB

# 170909s.
set.seed(scalar.seed.eric + 170909)
( runif(1) > 0.5 ) # BA

# 170915f.
set.seed(scalar.seed.eric + 170915)
( runif(1) > 0.5 )
