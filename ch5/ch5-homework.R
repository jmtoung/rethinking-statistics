library(rethinking)

# 5E1.
# 2, 4?

# 5E2.
# animal_diversity ~ latitude + plant_diversity
# animal_diversity ~ dnorm(mu, sigma)
# mu <- a + bL * latitude + bP * plant_diversity

# 5E3.
# time ~ funding + size

# 5E4.
# 1, 3, 4, 5

# 5M1.
# x_real is a normal distribution
trials <- 1e3
x_real <- rnorm(n= trials)
x_spurious <- rnorm(n=trials, mean=x_real)
y <- rnorm(n=trials, mean=x_real)

df <- data.frame(y=y, x_real=x_real, x_spurious=x_spurious)

# model: y ~ x_real + x_spurious

# 5M2.
rho <- .7
x_pos <- rnorm(trials)
x_neg <- rnorm(trials, rho*x_pos, sqrt(1-rho^2))
y <- rnorm(trials, xpos - x_neg)
d <- data.frame(y, x_pos, x_neg)

# 5H1.
data(foxes)
d <- foxes
# (1) body weight as a linear function of territory size (area)

# standardize predictor
d$area.s <- (d$area - mean(d$area))/sd(d$area)

# fit model
m5h1.1 <- map(
  alist(
    weight ~ dnorm( mu, sigma ),
    mu <- a + bA * area.s,
    a <- dnorm( 2, 10),
    bA ~ dnorm( 0, 1),
    sigma ~ dunif(0, 10)
  ) , data = d
)
precis(m5h1.1)
#       Mean StdDev  5.5% 94.5%
# a     4.53   0.11  4.35   4.7
# bA    0.02   0.11 -0.15   0.2
# sigma 1.18   0.08  1.06   1.3

# area.s is not a great predictor, slope lies in (-.15, 0.2)

# compute percentile interval of mean
area.seq <- seq(from=-3, to=3.5, length.out=30)
mu <- link(m5h1.1, data=data.frame(area.s=area.seq))
mu.PI <- apply(mu, 2, PI, prob=.95)

# plot it all
plot( weight ~ area.s, data=d, col=rangi2)
abline(m5h1.1)
shade(mu.PI, area.seq)

# (2) body weight as a linear function of groupsize

# standardize predictor
d$groupsize.s <- (d$groupsize - mean(d$groupsize))/sd(d$groupsize)

# fit model
m5h1.2 <- map(
  alist(
    weight ~ dnorm( mu, sigma ),
    mu <- a + bG * groupsize.s,
    a <- dnorm( 2, 10),
    bG ~ dnorm( 0, 1),
    sigma ~ dunif(0, 10)
  ) , data = d
)
precis(m5h1.2)
#        Mean StdDev  5.5% 94.5%
# a      4.53   0.11  4.36  4.70
# bG    -0.19   0.11 -0.36 -0.02
# sigma  1.16   0.08  1.04  1.29

# groupsize.s is an ok predictor, slope lies in (-0.36, -0.02)

# compute percentile interval of mean
groupsize.seq <- seq(from=-3, to=3.5, length.out=30)
mu <- link(m5h1.2, data=data.frame(groupsize.s=groupsize.seq))
mu.PI <- apply(mu, 2, PI, prob=.95)

# plot it all
plot( weight ~ groupsize.s, data=d, col=rangi2)
abline(m5h1.2)
shade(mu.PI, groupsize.seq)

# 5H2.
m5h2 <-map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bA * area.s + bG * groupsize.s,
    a <- dnorm( 5, 10),
    bA <- dnorm(0, 1),
    bG <- dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis( m5h2 )
#        Mean StdDev  5.5% 94.5%
# a      4.53   0.10  4.36  4.70
# bA     0.54   0.18  0.25  0.83
# bG    -0.63   0.18 -0.92 -0.34
# sigma  1.12   0.07  1.00  1.24

# area slope lies in (0.25, 0.83)
# groupsize slope lies in (-0.92, -0.34)
# both are good predictors
# different results because the effect of area and groupsize on weight are conditional on knowing the other variable
# e.g. once we know area, the effect of groupsize negatively predicts weight, and vice versa

### counterfactual plot for area

# prepare new counterfactual data
groupsize.avg <- mean(d$groupsize.s)
area.seq <- seq(from=-3, to=3, length.out=30)
pred.data <- data.frame(
  area.s=area.seq,
  groupsize.s=groupsize.avg
)

# compute counterfactual mean weight
mu <- link( m5h2, data=pred.data )
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# simulate counterfactual weight outcomes
R.sim <- sim(m5h2, data=pred.data, n=1e4)
R.PI <- apply(R.sim, 2, PI)

# display predictions, hiding raw data with type="n"
plot( weight ~ area.s, data=d, type="n")
mtext("groupsize.s = 0")
lines(area.seq, mu.mean)
shade(mu.PI, area.seq)
shade(R.PI, area.seq)

### counterfactual plot for groupsize

# prepare new counterfactual groupsize
area.avg <- mean(d$area.s)
groupsize.seq <- seq(from=-3, to=3, length.out=30)
pred.data <- data.frame(
  area.s=area.avg,
  groupsize.s=groupsize.seq
)

# compute counterfactual mean weight
mu <- link( m5h2, data=pred.data )
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# simulate counterfactual weight outcomes
R.sim <- sim(m5h2, data=pred.data, n=1e4)
R.PI <- apply(R.sim, 2, PI)

# display predictions, hiding raw data with type="n"
plot( weight ~ groupsize.s, data=d, type="n")
mtext("area.s = 0")
lines(groupsize.seq, mu.mean)
shade(mu.PI, groupsize.seq)
shade(R.PI, groupsize.seq)

# 5H3.
# (1) fit body weight as an additive function of avgfood and groupsize

# standardize predictor
d$avgfood.s <- (d$avgfood - mean(d$avgfood))/sd(d$avgfood)

m5h3.1 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bA * avgfood.s + bG * groupsize.s,
    a <- dnorm( 5, 10),
    bA <- dnorm(0, 1),
    bG <- dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis( m5h3.1 )
# Mean StdDev  5.5% 94.5%
# a      4.53   0.10  4.36  4.70
# bA     0.68   0.23  0.31  1.04
# bG    -0.79   0.23 -1.16 -0.43
# sigma  1.12   0.07  1.00  1.23

# avgfood slope lies between (.31, 1.04) - pretty good predictor
# groupsize slope lies between (-1.16, -.43) - pretty good predictor

# (2) fit body weight as an additive function of avgfood, groupsize, area

m5h3.2 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bA * avgfood.s + bG * groupsize.s + bR * area.s,
    a <- dnorm( 5, 10),
    bA <- dnorm(0, 1),
    bG <- dnorm(0, 1),
    bR <- dnorm(0, 1),
    sigma <- dunif(0, 10)
  ),
  data = d
)
precis( m5h3.2 )
# Mean StdDev  5.5% 94.5%
# a      4.53   0.10  4.37  4.69
# bA     0.44   0.27  0.00  0.87
# bG    -0.86   0.23 -1.23 -0.49
# bR     0.35   0.21  0.01  0.69
# sigma  1.10   0.07  0.99  1.22

# avgfood slope lies (0, 0.87) - effect is less than before
# groupsize slope lies (-1.23, -.49) - effect is slightly stronger
# area slope lies (.01, .69) - effect is not great (touches 0)

# (a) which is better, avg food or area?
# avg food is better because the slopes are further from 0 in the model `weight ~ avgfood + groupsize` than
# in the model `weight ~ area + groupsize`

# (b) area and avg food are probably correlated
