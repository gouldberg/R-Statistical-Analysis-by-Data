# setwd("//media//kswada//MyFiles//R//long_jump")
# setwd("//media//kswada//MyFiles//R//Bayesian_inference//long_jump")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\long_jump")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  long jump
# ------------------------------------------------------------------------------

N <- 25

x <- c(8.95, 8.68, 8.70, 8.74, 8.71, 8.58, 8.63, 8.48, 8.60, 8.65,
       8.41, 8.52, 8.53, 8.60, 8.60, 8.56, 8.66, 8.73, 8.74, 8.47,
       8.54, 8.35, 8.56, 8.51, 8.52)


data <- list(N = N, x = x)



# ------------------------------------------------------------------------------
# Scan stan code
# ------------------------------------------------------------------------------

scr <- ".//stan//long_jump.stan"

scan(scr, what = character(), sep = "\n", blank.lines.skip = F)




# ------------------------------------------------------------------------------
# Set parameters and Estimate
# ------------------------------------------------------------------------------

par <- c("mu", "sigma", "s", "p_w", "x_99", "u_rover100", "r_w", "xpred", "p_new") 

war <- 5000

ite <- 10000

see <- 1234

dig <- 3

cha <- 4



fit <- stan(file = scr, model_name = "long_jump", data = data, pars = par, verbose = F, seed = see, chains = cha, warmup = war, iter = ite)



# ------------------------------------------------------------------------------
# plot traceplot
# ------------------------------------------------------------------------------

traceplot(fit)



# ------------------------------------------------------------------------------
# plot posterior distribution
# ------------------------------------------------------------------------------

plot(fit)


print(fit, pars = par, digits_summary = dig)



# ----------
# most-likely world record at each year;   8.542 m

# 95% of credible interval is:  8.493 - 8.594 m

# standard deviation of each year's world record:  0.155 m  --> very small !!  its hard competition !!

# Mike Powell's world record of 8.95 m is 96.3% at distribution

# Average world record that appears once in 100 years is: 9.100 m

# The probability that Mike Powell's world record 8.95 m is once in 100 years is:  3.9%

# Mike Powell's world record appears once in 39.417 years in average

# The probability that Mike Powell's world record will be overcome next year is 3.7%

# Average world record in next year is 8.613 m
