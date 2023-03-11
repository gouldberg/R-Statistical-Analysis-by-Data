# setwd("//media//kswada//MyFiles//R//bailey_density")
setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ----------
# missing values for the spatial coordinates to be removed
colSums(is.na(DF1))


DF  <- na.exclude(DF1)



# ----------
M1 <- glm(TotAbund ~ MeanDepth, data = DF, family = poisson(link = "log"))

M2 <- glm(TotAbund ~ MeanDepth * fPeriod,  data = DF,  family = poisson)

DF$LogSA <- log(DF$SweptArea)

M3 <- glm(TotAbund ~ MeanDepth * factor(Period) + offset(LogSA), data = DF,  family = poisson)



# ------------------------------------------------------------------------------
# Calculation of residual deviance for M1
# ------------------------------------------------------------------------------

beta1 <- coef(M1)[1]

beta2 <- coef(M1)[2]

eta <- beta1 + beta2 * DF$MeanDepth

mu  <- exp(eta)

y <- DF$TotAbun



# ----------
# residual deviance = twice the difference between the log likelihood of a model that provides a perfect fit and the model under study

sum(y * log(y/mu) - (y - mu))

2 * sum(y * log(y/mu) - (y - mu))

M1$deviance



# ----------
# loglikelihood
logLik(M1)

sum(DF$TotAbun * eta - mu - lgamma(DF$TotAbun + 1))
