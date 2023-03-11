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
# Run Stan's variational algorithm for approximate posterior sampling
# ------------------------------------------------------------------------------

# construct a Stan model
model <- stan_model(scr)


fit_vb <- vb(model, data=data, pars=par, output_samples=1000, seed=see)



# ----------
traceplot(fit_vb)


print(fit_vb, pars="theta", digits_summary=dig)


