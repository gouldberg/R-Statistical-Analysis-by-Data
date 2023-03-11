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
# Heterogeneous Negative Binomial regression
# ------------------------------------------------------------------------------

library(msme)

# A feature of the nbinomial function is that it allows for the dispersion parameter alpha to be parameterized.
# That is, coefficients can be determined for the dispersion parameter.

mod.nbH <- nbinomial(TotAbund ~ MeanDepth + fPeriod,
                 formula2 = ~ MeanDepth + fPeriod,
                 offset = DF$LogSA,
                 family = "negBinomial",
                 mean.link = "log",
                 scale.link = "log_s",
                 data = DF)

summary.msme(mod.nbH)



# -->
# None of the regression parameters for the dispersion coefficient are significant.
# This is not surprising, since the negative binomial dispersion statistic is close to 1.0.
# The negative binomial appropriately adjusted for the overdispersion in the Poisson model, and is itself well-fitted.
# If Mean Depth, for instance, had been a significatn predictor of the dispersion, it would mean that 
# Mean Depth is providing the model with additional variation, and this would have been reflected in the dispersion parameter.
