# 7E1.
# 1. strain of yeast, temperature
# 2. location
# 3. diesel

# 7E2.
# 2. Speed = B * NumCylinders + I * Injector
# 3. Beliefs = P * Parents + F * Friends

# 7M1.
# B ~ Normal(mu, sigma)
# mu = alpha + Bw * W + Bs * S + Bws * W * S
#

# Adding temperature ...
# mu = alpha + Bw * W + Bs * S + Bt * T + Bws * W * S + Bwt * W * T + Bst * S * T + Bwst * W * S* T

# 7M2.
# temperature hot => 0, temperature cold => 1
# mu = T ( alpha + Bw * W + Bs * S + Bws * W * S)
