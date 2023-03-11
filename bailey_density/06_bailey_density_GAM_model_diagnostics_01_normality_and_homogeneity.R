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
# Model validation:  normality and homogeneity
# residuals vs. fitted values
# ------------------------------------------------------------------------------

# residuals vs. fitted values
par(mfrow = c(1, 1))

plot(resid(mod_obj) ~ fitted(mod_obj), xlab = "Fitted values", ylab = "Residuals")
lines(lowess(fitted(mod_obj), resid(mod_obj)), col = "blue", lty = 2)
abline(h = 0, v = 0, lty = 2, col = "darkgray")



# -->
# It shows non-normality and heterogeneity



# ------------------------------------------------------------------------------
# Model validation:  normality and homogeneity
# distribution of standardized residuals
# ------------------------------------------------------------------------------

# density plot and histogram

par(mfrow = c(1, 2), mar = c(5, 5, 3, 3))

hist(resid(mod_obj), breaks = 25, main = "", xlab = "Residuals")
car::densityPlot(resid(mod_obj))




