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
# The quasi-poisson model
# ------------------------------------------------------------------------------

M2.qpois <- glm(TotAbund ~ MeanDepth + factor(Period) + offset(LogSA), data = DF, family = quasipoisson)


summary(M2.qpois)


# -->
# dispersion parameter is 120.1253



plot(M2.qpois)

plot_resid(data = DF, mod=M2.qpois, y = "TotAbund")



# ------------------------------------------------------------------------------
# negative-binomial model
# ------------------------------------------------------------------------------

library(MASS)


# ----------
# estimate theta first
( nbin <- glm.nb(TotAbund ~ MeanDepth + fPeriod + offset(LogSA), data = DF) )

( theta <- nbin$theta )


# -->
# dispersion parameter is 1.953821
# Far better than Poisson model



# ----------
mod.nbin <- glm(TotAbund ~ MeanDepth + fPeriod + offset(LogSA), data = DF, family = negative.binomial(theta))

summary(mod.nbin)



# ----------
library(effects)

plot(allEffects(mod.nbin), band.colors = "blue", lwd=3, ylab = "Number of articles", main="", ylim=c(0, log(1000)))

plot_resid(data = DF, mod = mod.nbin, y = "TotAbund")

