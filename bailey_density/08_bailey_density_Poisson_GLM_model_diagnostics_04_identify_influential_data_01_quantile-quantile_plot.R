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



# ----------
mod_obj <- M3



# ------------------------------------------------------------------------------
# Normal Quantile-quantile Plot:  Studentized residuals vs. Normal Quantiles
# ------------------------------------------------------------------------------

car::qqPlot(rstudent(mod_obj), xlab = "Normal Quantiles", ylab = "Studentized residuals")


# -->
# Not fitted to Poisson GLM



# ----------
# by group
car::qqPlot(rstudent(mod_obj), group = DF$fPeriod, xlab = "Normal Quantiles", ylab = "Studentized residuals")




# -->
# For GLMs with discrete responses, such plots are often disappointing, even with a reasonably good-fitting model, because
# (a) possible outlierrs can appear at both the lower and upper ends of the distribution of residuals
# (b) the theoretical normal distribution used to derive the envelope may not be well approximated in a given model


