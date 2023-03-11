setwd("//media//kswada//MyFiles//R//iatfaces")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  iatfaces
# ------------------------------------------------------------------------------

data("iatfaces", package = "MPsychoR")


str(iatfaces)


p1dat <- subset(iatfaces, id == 1)
p2dat <- subset(iatfaces, id == 2)
p3dat <- subset(iatfaces, id == 3)
p4dat <- subset(iatfaces, id == 4)



# ------------------------------------------------------------------------------
# Fit HMM with covariate block, 2-state model, transition probabilities as function of covariate
#   - model the transition probabilities as a function of the block condition z(t)
#     logit(1 - a11) = eta0(1) + eta1(1) * z(t)   (a11: transition probability from state 1 to 1)
#     logit(a22) = eta0(2) + eta1(2) * z(t)       (a22: transition probability from state 2 to 2)
# ------------------------------------------------------------------------------

library(depmixS4)


set.seed(123)

p1obj4 <- depmix(log(latency) ~ 1, data = p1dat, nstates = 2, transition = ~ block)

p1fit2c <- fit(p1obj4, emcontrol = em.control(maxit = 5000))



# ----------
summary(p1fit2c)

summary(p1fit2c, which = "transition")



# -->
# We canget one set of parameters fro the congruent condition
# One set of parameters for the incongruent condition.

# The output reports the eta-parameters on a logit scale.
# "Probabilities at zero values of the covariates": the eta parameters are converted into probabilities given the covariate values is zero.
# (here, congruent block due to dummy coding)



# ----------
# depmixS4::getpars()
getpars(p1fit2c)

eta1 <- matrix(getpars(p1fit2c)[3:6], 2, byrow = TRUE)
eta2 <- matrix(getpars(p1fit2c)[7:10], 2, byrow = TRUE)

eta1
eta2



# ----------
# a11, how likely person 1 stays in state 1 when block = congruent
exps1b1 <- exp(eta1[1,])
a11c <- 1/sum(exps1b1)
a11c


# a11, how likely person 1 stays in state 1 when block = incongruent
exps1b2 <- exp(eta1[2,])
a11ic <- 1/sum(exps1b2)
a11ic


# a22, how likely person 1 stays in state 2 when block = congruent
exps2b1 <- exp(eta2[1,])
a22c <- 1 - 1/sum(exps2b1)
a22c


# a22, how likely person 1 stays in state 1 when block = incongruent
exps2b2 <- exp(eta2[2,])
a22ic <- 1 - 1/sum(exps2b2)
a22ic



# ----------
Acong <- round(matrix(c(a11c, 1 - a11c, 1 - a22c, a22c), 2, byrow = TRUE), 5)

Aicong <- round(matrix(c(a11ic, 1 - a11ic, 1 - a22ic, a22ic), 2, byrow = TRUE), 5)

dimnames(Acong) <- dimnames(Aicong) <- list(c("fromS1", "fromS2"), c("toS1", "toS2"))

Acong

Aicong


# -->
# These results show again that the person's state switching behavior is almost fully determined by the block condition.
# We can interpret state 1 as the "congruent state" and state 2 as the "incongruent state", for person 1.




