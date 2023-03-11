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
# Negative Binomial regression by direct dispersion parametrization
# ------------------------------------------------------------------------------

# The function nbinomial uses the direct parameterization, alpha, of the dispersion parameter.
library(msme)

mod.nbin2 <- nbinomial(TotAbund ~ MeanDepth + fPeriod + offset(LogSA), data = DF)


summary(mod.nbin2)
summary(mod.nbin)

mod.nbin2$dispersion


# -->
# dispersion parameter is 0.8596 ... why small ...


# -->
# (Intercept)_s = 0.824 ??
# an additional advantage of nbinomial is that the dispersion statistic (Residual Pearson / residual df) is calculated


