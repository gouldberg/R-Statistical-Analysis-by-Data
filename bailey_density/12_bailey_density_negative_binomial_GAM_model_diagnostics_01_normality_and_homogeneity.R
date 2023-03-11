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
# Model validation:  normality and homogeneity
# residuals vs. fitted values
# ------------------------------------------------------------------------------

# residuals vs. fitted values
par(mfrow = c(1, 1))

plot(resid(mod_obj) ~ fitted(mod_obj), xlab = "Fitted values", ylab = "Residuals")
lines(lowess(fitted(mod_obj), resid(mod_obj)), col = "blue", lty = 2)
abline(h = 0, v = 0, lty = 2, col = "darkgray")



# -->
# close to normality !!



# ------------------------------------------------------------------------------
# Model validation:  normality and homogeneity
# distribution of standardized residuals
# ------------------------------------------------------------------------------

# density plot and histogram

par(mfrow = c(1, 2), mar = c(5, 5, 3, 3))

hist(resid(mod_obj), breaks = 25, main = "", xlab = "Residuals")
car::densityPlot(resid(mod_obj))
