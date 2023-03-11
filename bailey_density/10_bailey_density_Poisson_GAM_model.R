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




# ------------------------------------------------------------------------------
# Poisson GAM with offset
# ------------------------------------------------------------------------------

# We assume a Poisson distribution for total abundance, and the predictor function contains two smoothers (one per period)

mod.pgam <- mgcv::gam(TotAbund ~ s(MeanDepth, by = fPeriod) + fPeriod + offset(LogSA), family = poisson,  data = DF)


summary(mod.pgam)


# -->
# In a Gaussian GAM, cross validation is applied to find the optimal amount of smoothing for a smoother.
# In this process the generalized cross-validation (GCV) is used. This is because somewhere in the underlying equations there is an
# unknown variance term for the residuals.
# When a Poisson distribution is used, there is no extra variance parameter, and the so-called Un-Biased Risk Estimator (UBRE) is used by the 
# cross validation process to find the optimal amount of smoothing.
# However, if you use family = quasiPoisson, an extra parameter is estimated for the variance, and teh code will use GCV again.




# ----------
# Calcualte overdispersion
( phi <- sum(residuals(mod.pgam, type = "pearson")^2 / mod.pgam$df.residual) )


# -->
# By allowing for a non-linear depth effect, the overdispersion is reduced compared to a Poisson GLM with offset,
# from 121.69 to 100.43.



# ------------------------------------------------------------------------------
# Plot smoothers
# ------------------------------------------------------------------------------

# To plot the smoothers
par(mfrow = c(1,2))
plot(mod.pgam, shade = TRUE)



# ----------
par(mfrow = c(2,2))
plot(mod.pgam, all.terms = TRUE)



# ----------
par(mfrow = c(1,2))
plot(mod.pgam,select = c(1), main ="Period 1")
plot(mod.pgam,select = c(2), main ="Period 2")



# ----------
par(mfrow = c(2,2))
plot(mod.pgam, all.terms = TRUE, resid = TRUE, scale = FALSE)



# ----------
par(mfrow = c(1,2))
plot(mod.pgam, shade = TRUE, scale = FALSE)

