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
library(mgcv)
mod.nbgam <- mgcv::gam(TotAbund ~ s(MeanDepth, by = fPeriod) + fPeriod + offset(LogSA), family = nb(), data = DF)

mod_obj <- mod.nbgam




# ------------------------------------------------------------------------------
# Model validation:  independence
# ------------------------------------------------------------------------------

par(mfrow = c(1, 2), mar = c(5, 5, 3, 3))

boxplot(resid(mod_obj) ~ factor(DF$Period), xlab = "Period", ylab = "Residuals")
abline(h = 0)

car::densityPlot(resid(mod_obj), g = factor(DF$Period), xlab = "Period", ylab = "Residuals")




