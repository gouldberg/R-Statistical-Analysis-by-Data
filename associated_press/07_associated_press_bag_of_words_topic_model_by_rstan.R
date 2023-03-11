# setwd("//media//kswada//MyFiles//R//associated_press//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//associated_press//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Associated Press (AP)
#   - the partial sample from AP documents (from "topicmodels")
# ------------------------------------------------------------------------------

source("associated_press.R")

# number ot topics
K

# number of words
V

# number of documents
D

# data size
N


wordID
length(unique(wordID))


freq


docID



# ----------
# topics prior distritbuion
alpha

# word prior distribution
beta


data <- list(K=K, V=V, D=D, N=N, w=wordID, Freq=freq, doc=docID, alpha=alpha, beta=beta)



# ------------------------------------------------------------------------------
# Set cores
# ------------------------------------------------------------------------------

parallel::detectCores()

options(mc.cores = parallel::detectCores())



# ------------------------------------------------------------------------------
# scan stan code
# ------------------------------------------------------------------------------

scr <- ".//stan//associated_press.stan"

scan(scr, what = character(), sep = "\n", blank.lines.skip = F)



# ------------------------------------------------------------------------------
# set parameters
# ------------------------------------------------------------------------------

par <- c("theta", "phi")

war <- 1000

ite <- 2000

see <- 3456

dig <- 3


# -----------
# In the topic model, each topic is considered as unordered categorical value.
# If applying multiple MCMC chains, the topic assigned to each word in each chain will be different.
# Therefore, we only apply 1 MCMC chain

cha <- 1



# ------------------------------------------------------------------------------
# Estimate:  ###########  DO NOT RUN, IT TAKES 50 min !!!
# ------------------------------------------------------------------------------


# IT TAKES TIME !!!:  50 min.
fit <- stan(file=scr, model_name="associated_press", data=data, pars=par, verbose=F, seed=see, chains=cha, warmup=war, iter=ite)



# ------------------------------------------------------------------------------
# plot traceplot
# ------------------------------------------------------------------------------

traceplot(fit)



# ------------------------------------------------------------------------------
# plot posterior distribution
# ------------------------------------------------------------------------------

print(fit, pars=par, digits_summary=dig)


# -->
# theta shows document * topic probability



