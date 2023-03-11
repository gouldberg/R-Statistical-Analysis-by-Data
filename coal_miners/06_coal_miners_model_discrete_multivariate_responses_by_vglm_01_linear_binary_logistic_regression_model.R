setwd("//media//kswada//MyFiles//R//coal_miners")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coal Miners
# ------------------------------------------------------------------------------

data("CoalMiners", package = "vcd")

data <- CoalMiners



# ----------
# Breathlessness * Wheeze * Age
dim(data)

dimnames(data)


data


# ----------
# Age 20-24 to be omitted from analysis here
dat2 <- data[,,2:9]


structable(. ~ Age, data = dat2)




# ------------------------------------------------------------------------------
# model discrete multivariate reponses by vglm
# ------------------------------------------------------------------------------

coalminers <- data.frame(t(matrix(aperm(CoalMiners, c(2, 1, 3)), 4, 9)))


colnames(coalminers) <- c("BW", "Bw", "bW", "bw")


coalminers$age <- c(22, 27, 32, 37, 42, 47, 52, 57, 62)


coalminers <- transform(coalminers, agec = (age - 42) / 5)


coalminers$Age <- dimnames(CoalMiners)[[3]]


coalminers




# ----------

library(VGAM)


# binom2.or():  binary logistic models, allowing some or all of the logits or odds ratio submodels to be
# constrained to be intercept-only and
# the two margianl distributions can be constrained to be equal.


# eta1 = logit pi(1*)   eta2 = logit pi(*1)   eta12 = log( (pi(11) * pi(12)) / (pi(12) * pi(21)) ) = log(theta)
# zero = NULL:  none of the linear predictors (eta1, eta2, eta12) are modeled as constants
# the default, zero = 3

cm.vglm1 <- vglm(cbind(bw, bW, Bw, BW) ~ agec, binom2.or(zero = NULL), data = coalminers)


cm.vglm1



summary(cm.vglm1)





# ------------------------------------------------------------------------------
# test residual deviance of the model against the saturated model
# ------------------------------------------------------------------------------

G2 <- deviance(cm.vglm1)


1 - pchisq(deviance(cm.vglm1), cm.vglm1@df.residual)





# ------------------------------------------------------------------------------
# estimated coefficient
# ------------------------------------------------------------------------------

coef(cm.vglm1, matrix = TRUE)



# value of odds
exp(coef(cm.vglm1, matrix = TRUE))



# -->
# the odds of a miner showing breathlessness are multiplied by 1.67, a 67% increase,
# for each 5 years increase in age
# the odds of wheeze are multiplied by 1.38, a 38% increase.

# The odds ratio for the association between the two symptoms are multiplied by 0.88, a 12% decrease over each 5-year interval.

