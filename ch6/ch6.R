library(rethinking)

# 6.1
sppnames <- c( "afarensis","africanus","habilis","boisei",
               "rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

# 6.2
m6.1 <- lm( brain ~ mass, data =d )

# 6.3
1 - var(resid(m6.1))/var(d$brain)
summary(m6.1) # the above equals "Multiple R-squared"

# 6.4
m6.2 <- lm( brain ~ mass + I(mass^2), data=d)

# 6.15
data(cars)
m <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,30)
  ) , data=cars )
post <- extract.samples(m,n=1000)

# 6.16
n_samples <- 1000
ll <- sapply( 1:n_samples ,
              function(s) {
                mu <- post$a[s] + post$b[s]*cars$speed
                dnorm( cars$dist , mu , post$sigma[s] , log=TRUE )
              } )

# 6.17
n_cases <- nrow(cars)
# Note: understanding `log_sum_exp`
# Formula for `lppd` is the sum of `log(Pr(y_i))` for each observation i
# Pr(y_i) is the average likelihood (probability) of observation i in the training sample.
# So we want to average the likelihoods for each observation, take the log, and then sum all log values across all observations
# However, we would like to do things on the log-scale for numerical stability.
# In essence, if there are three samples in the posterior distribution, for observation i, we want to compute Pr(y_i) = log(p1+p2+p3), where p1 is the likelihood of observation i for sample 1, etc.
# The formula in `log_sum_exp` is:
# {
#    xmax <- max(x)
#    xsum <- sum(exp(x - xmax))
#    xmax + log(xsum)
# }
#
# As you can see in 6.16, we compute the log-likelihood. This is what we pass in to `log_sum_exp`.
# So essentially we are computing log(log(p1) + log(p2) + log(p3)). The formula calls for identifying
# the largest log(p_i). Let's just designate that as log(p1). What `log_sum_exp` returns is:
# log(p1) + log( exp(log(p1) - log(p1)) + exp(log(p2) - log(p1)) + exp(log(p3) - log(p1)) )
# This simplifies to log(p1) + log( exp(0) + exp(log(p2))/exp(log(p1)) + exp(log(p3))/exp(log(p1))).
# This further simplifies to log(p1) + log(1 + exp(log(p2/p1)) + exp(log(p3/p1)))
# Which simplifies to log(p1) + log(1 + p2/p1 + p3/p1).
# This finally simplifies to log(p1 * (1 + p2/p1 + p3/p1)) = log(p1 + p2 + p3) which is exactly what we want to compute.
lppd <- sapply( 1:n_cases , function(i) log_sum_exp(ll[i,]) - log(n_samples) )

# 6.18
pWAIC <- sapply( 1:n_cases , function(i) var(ll[i,]) )

# 6.19
-2*( sum(lppd) - sum(pWAIC) )

# 6.20
waic_vec <- -2*( lppd - pWAIC )
sqrt( n_cases*var(waic_vec) )
