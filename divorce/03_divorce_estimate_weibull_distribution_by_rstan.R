# setwd("//media//kswada//MyFiles//R//divorce//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//divorce//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  divorce
# ------------------------------------------------------------------------------

source("divorce_data.R")

length(dat)

data.ls <- list(J = length(dat), tt = dat)

table(dat)




# ------------------------------------------------------------------------------
# Set cores
# ------------------------------------------------------------------------------

# parallel::detectCores()

# options(mc.cores = parallel::detectCores())



# ------------------------------------------------------------------------------
# scan stan code
# ------------------------------------------------------------------------------

scr <- ".//stan//divorce.stan"

scan(scr, what = character(), sep = "\n", blank.lines.skip = F)



# ------------------------------------------------------------------------------
# set parameters
# ------------------------------------------------------------------------------

# par <- c("m", "eta", "P", "mu", "mode")

par <- c("m", "eta", "mu", "mode")

war <- 2500

ite <- 5000

see <- 1234

dig <- 3

cha <- 4



# ------------------------------------------------------------------------------
# Estimate
# ------------------------------------------------------------------------------

# IT TAKES TIME (by PC):  3 - 4 min.
fit <- stan(file=scr, model_name="divorce", data=data.ls, pars=par, verbose=F, seed=see, chains=cha, warmup=war, iter=ite)



# ------------------------------------------------------------------------------
# plot traceplot
# ------------------------------------------------------------------------------

traceplot(fit)



# ------------------------------------------------------------------------------
# plot posterior distribution
# ------------------------------------------------------------------------------

print(fit, pars=par, digits_summary=dig)



# -->
# very close to gamlss estimation



