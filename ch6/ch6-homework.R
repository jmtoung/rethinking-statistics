library(rethinking)

# 6E1.
# 1. continuous
# 2. increasing as number of possible events increases.
# 3. additive

# 6E2.
-0.7*log(.7) -0.3*log(.3)
# 0.6108643

# 6E3.
sum(sapply(c(.2, .25, .25, .3), function(x) { -x * log(x)}))
# 1.376227

# 6E4.
sum(sapply(c(0, 1/3, 1/3, 1/3), function(x) { -x * log(x)}), na.rm=TRUE)
# 1.098612

# 6M1.
# AIC = D_train + 2p, where p = # of parameters

# DIC = D_bar _ (D_bar + D_hat) = D_bar + p_D, where p_D = (D_bar + D_hat)
# D is the posterior distribution of deviance. D_bar is the average of D. D_hat is the deviance computed at the posterior mean (e.g. compute the average of each parameter in posterior distribution).
# p_d is analogous to the number of parameters used in computing AIC

# WAIC = -2 * (lppd - p_WAIC)
# lppd = sum_i_N( log(Pr(y_i))), where Pr(y_i) is the average likelihood of observation i in training sample
# p_WAIC is the variance in log-likelihood for observation i in the training sample.

# 6M2.
# Model selection vs Model averaging.
# Model selection - choosing model with lowest AIC/DIC/WAIC value and discarding the others. This discards the information about relative model accuracy. Relative model accuracy provides advice aobut how confident we might be about models conditional on the set of models compared.

# Model averaging - using DIC/WAIC to construct a posterior prediction distribution that explots what we know about relative accuracy of the models. Does not mean averaging parameter estimates. Better to think of it as prediction averaging. Use WAIC weights to weight predictions from each model.

# 6H1.
data(Howell1)
d <- Howell1
d$age <- (d$age - mean(d$age))/sd(d$age)
set.seed( 1000 )
i <- sample(1:nrow(d),size=nrow(d)/2)
d1 <- d[ i , ]
d2 <- d[ -i , ]

