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
M5 <- mgcv::gam(Dens ~ s(MeanDepth), data = DF)
M3 <- gamm(SQ.Dens ~ s(MeanDepth, by = factor(Period)) + factor(Period), weights = varIdent(form =~ 1 | IMD), method = "REML", data = DF)


mod_obj <- M5
mod_obj <- M3$gam



# ------------------------------------------------------------------------------
# Model validation:  independence
# ------------------------------------------------------------------------------

# plot residuals ve. each covariate and covariate not in the model
par(mfrow = c(1, 2), mar = c(5, 5, 3, 3))

boxplot(resid(mod_obj) ~ factor(DF$Period), xlab = "Period", ylab = "Residuals")
abline(h = 0)

car::densityPlot(resid(mod_obj), g = factor(DF$Period), xlab = "Period", ylab = "Residuals")



# -->
# for M5

# Period = 1:  trawls taken 1979 - 1989  and  Period = 2:  sampling 1997 - 2002
# Nearly all residuals from sampling Period 2 are negative, with a few positive.
# This is a clear indication that the covariate "period" should be added to the model


