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
# Transforming the density data
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ Dens, data = DF)



# ----------
bc <- caret::BoxCoxTrans(DF$Dens)

bc


# -->
# estimated lambda is 0.2



# ----------
# transforming for symmetry
car::symbox(~ Dens, data = DF)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "lifespan" untransformed is inconsistent with the goal of making the variabel normally distributed.
p1 <- car::powerTransform(Dens ~ 1, data = DF, family = "bcPower")

summary(p1)


# should use lambda = 0.2048  (p-value is not small)
car::testTransform(p1, lambda = 0.2048)



# ------------------------------------------------------------------------------
# Apply a square root transformation
# ------------------------------------------------------------------------------

DF$SQ.Dens <- sqrt(DF$Dens)

M10 <- gam(SQ.Dens ~ s(MeanDepth, by = fPeriod) + fPeriod, data=DF)


summary(M10)

plot(M10)



# ------------------------------------------------------------------------------
# Apply a log transformation
# ------------------------------------------------------------------------------

DF$L.Dens <- log(DF$Dens)

M10_l <- gam(L.Dens ~ s(MeanDepth, by = fPeriod) + fPeriod, data=DF)


summary(M10_l)

plot(M10_l)



# ------------------------------------------------------------------------------
# Model validation:  normality and homogeneity
# ------------------------------------------------------------------------------

mod_obj <- M10
mod_obj <- M10_l


# residuals vs. fitted values
par(mfrow = c(1, 1))

plot(resid(mod_obj) ~ fitted(mod_obj), xlab = "Fitted values", ylab = "Residuals")
lines(lowess(fitted(mod_obj), resid(mod_obj)), col = "blue", lty = 2)
abline(h = 0, v = 0, lty = 2, col = "darkgray")



# ----------
# density plot and histogram

par(mfrow = c(1, 2), mar = c(5, 5, 3, 3))

hist(resid(mod_obj), breaks = 25, main = "", xlab = "Residuals")
car::densityPlot(resid(mod_obj), g = DF$fPeriod)



# -->
# logarithmic transformation solved the heterogeneity
# BUT THE DISADVANTAGE OF TRANSFORMING DATA IS THAT IT MAY REMOVE ANY INTERACTIONS PRESENT.

