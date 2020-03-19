library(rethinking)

# 8E1.
# (3) must be symmetric
#
# 8E2.
# adaptive proposals
#
# 8E3.
# n_eff is crude estimate of the number of independent samples you get
#

# 8H1.
mp <- map2stan(
  alist(
    a ~ dnorm(0, 1),
    d ~ dcauchy(0, 1)
  ),
  data=list(y=1),
  start=list(a=0, b=0),
  iter=1e4, warmup=100, WAIC=FALSE
)
