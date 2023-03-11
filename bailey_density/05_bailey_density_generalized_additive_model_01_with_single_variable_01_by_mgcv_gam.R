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
# Applying simple GAM with mgcv package
#   - mgcv package allows for automatic selection of the amount of smoothing
#   - temporal and spatial correlation structures can be added
#   - variance covariates can be added to model heterogeneity
#   - random effects can be used.
#   - advanced smoothers, such as thin plate regression splines, cubic regression splines, and P-splines, and one can also specify the smoothers as random effects.
#
#   - But for many data sets it would be difficult to perceive any differences between a LOESS smoother and a thin plate regression spline.
# ------------------------------------------------------------------------------

# gam function employs a so-called thin plate regression spline.
# Although the resulting pattern is similar to that of a LOESS smoother,
# the essential mathematics are more complicated.

# degree of freedom is fixed at 4, similar to 3rd-order polynomial function

M4 <- mgcv::gam(Dens ~ s(MeanDepth, fx =TRUE, k = 5), data = DF)


summary(M4)


anova(M4)



# -->
# effective degrees of freedom (edf): a calibration value that determines the shape of the curve.
# A value of 1 means that the smoother will be a straight line, whereas a value of 10 produces a highly non-linear pattern.
# The value of 4 is used by some older software packages as default, and the resulting smoother looks roughly similar to the pattern of a 
# third order polynomial function.


# the smoother is highly significant from anova() F-test
# Note that anova for GAM does not perform sequential testing. 
# The anova function is useful if there is a categorical covariate of more than 2 levels and one p-value for the covariate is needed.



# ------------------------------------------------------------------------------
# Estimated Smoother for mean depth
# ------------------------------------------------------------------------------

# The horizontal axis shows the depth gradient and the vertical axis the smoother f(MeanDepth).
# To obtain the fitted values, the estimated value of the intercept (0.00471) is added.
# This means that the three (solid line and two dotted lines) curves can be shifted upward by 0.00471

# Dens(i) = 0.0047 + f(Depth(i))
# the funciton f(Depth(i)) is the solid line.

par(mar = c(5,5,3,3))

plot(M4, cex.lab = 1.5, shade = TRUE)




# ------------------------------------------------------------------------------
# Fitted Value
# ------------------------------------------------------------------------------

# fitted value adding estimated value for the intercept to the smoother

MyData4 <- data.frame(MeanDepth = seq(from = 800, to = 4865, length = 100))

P4 <- predict(M4, newdata = MyData4, se = TRUE)



# ----------
par(mar = c(5,5,3,3))

plot(x = DF$MeanDepth, y = DF$Dens, xlab = MyXLab, ylab = MyYLab)

# add standard errors
lines(x = MyData4$MeanDepth, y = P4$fit, lwd=3)
lines(x = MyData4$MeanDepth, y = P4$fit + 2 * P4$se.fit, lwd = 2, lty = 2)
lines(x = MyData4$MeanDepth, y = P4$fit - 2 * P4$se.fit, lwd = 2, lty = 2)




# ----------
# or we can use shift option
# Shifted by intecept:  = Dens(i) = 0.0047 (=intercept) + f(Depth(i))

plot(M4, shift = coef(M4)[1])

