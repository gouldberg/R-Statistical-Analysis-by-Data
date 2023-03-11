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
# fit a linear regression
# ------------------------------------------------------------------------------

# The estimated slope is small, since depth is expressed in metres.
M1 <- lm(Dens ~ MeanDepth, data = DF)

print(summary(M1), digits = 2, signif.stars = FALSE)



# -->
# The model explains 34% of the variation in the density data, and the regression parameter for depth is highly significant.



# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)

eff <- effects::allEffects(M1)

# ----------
# plot main effets of each variable (default is reponse ratio)
# plot(eff)
plot(car::predictorEffects(M1))

plot(eff, type = "response")



# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
plot(predictorEffects(M1, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))
# eff2 <- effects::allEffects(M1, partial.residuals = TRUE)
# plot(eff2, rows = 2, cols = 2, type = "response", residuals.pch = 15)



# ------------------------------------------------------------------------------
# Pearson residual, Standardized residuals
# ------------------------------------------------------------------------------

residualPlots(M1)


# residualPlots(M1, type = "rstandard")



# -->
# There is a clear violation of homogeneity and also a patten of clustered residuals above zero line and 
# clustered residuals below the zero line, especially around 4000 m.



# ----------
# residuals by group
residualPlots(M1, groups = DF$Year >= 1990)



# ----------
# by index
# standardized Pearson residuals
stand.resid <- rstandard(M1, type = "sd.1")

par(mfrow=c(1,1))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized Pearson residuals", type = "h")

abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))




# ------------------------------------------------------------------------------
# Non-constant Variacen Score Test
# ------------------------------------------------------------------------------

car::ncvTest(M1)




# ------------------------------------------------------------------------------
# fitted linear regression line
# ------------------------------------------------------------------------------

par(mfrow = c(1,1), mar = c(5,5,2,2))

plot(x = DF$MeanDepth, y = DF$Dens, cex.lab = 1.5, xlab = MyXLab , ylab = MyYLab)

abline(M1,lwd = 5)



# -->
# Note that at some depth ranges, all values are under- or over-fitted.
# We also have negative fitted density values for greater depths.
# This means that if this model were used for predictions, we could predict negative fish densities !



# ------------------------------------------------------------------------------
# Standardized Pearson Residual with curvature test
# ------------------------------------------------------------------------------

# residuals against each predictor to check systematic variation
# quadratic = TRUE:  computes a curvature test for each of the plots by adding a quadratic term and testing the quadratic to be zero

car::residualPlot(M1, "MeanDepth", type = "rstandard", 
                  quadratic = TRUE, col.quad = gray(0.7), ylim = c(-5, 5))



# ------------------------------------------------------------------------------
# Normal Quantile-quantile Plot:  Studentized residuals vs. Normal Quantiles
# ------------------------------------------------------------------------------

car::qqPlot(rstudent(M1), xlab = "Normal Quantiles", ylab = "Studentized residuals")


car::qqPlot(rstudent(M1), groups = DF$Year > 1990, xlab = "Normal Quantiles", ylab = "Studentized residuals")



# ------------------------------------------------------------------------------
# Studentized Residuals
#  - id.n argument here works to label that number of the most extreme observations individually for each measure plotted.
# ------------------------------------------------------------------------------

car::influenceIndexPlot(M1, vars = c("studentized"), id.n = 4)



# ------------------------------------------------------------------------------
# car::OutlierTest()
#  - give a formal test of significance of the largest absolute studentized residuals, witha Bonferroni-adjusted p-value accounting
#    for choosing the largest values among n such tests.
# ------------------------------------------------------------------------------

car::outlierTest(M1)

  

# ------------------------------------------------------------------------------
# Studentized (deletion) residuals vs. leverage, showing the value of Cook's distance by the area of the bubble symbol
# ------------------------------------------------------------------------------

influencePlot(M1)



# ----------
# An extended version is produced by the function car::influencePlot(),
# which additionally uses the size (area) of the plotting symbol to also show the value of Cook's D.

op <- par(mar = c(5, 4, 1, 1) + .1, cex.lab = 1.2)
res <- car::influencePlot(M1, id.col = "blue", scale = 8, id.cex = 1.5, id.n = 3)
k <- length(coef(M1))
n <- nrow(M1)
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)



# ----------
# show data together with diagnostics for influential cases
# idx <- which(rownames(M1) %in% rownames(res))

cbind(DF[as.numeric(rownames(res)), c("TotAbund", "MeanDepth", "Dens")], res)



# -->
# the largest Cook's D is only 0.179 for case 23 ...



# ------------------------------------------------------------------------------
# The collection of index plot
#  - plots various influence diagnostics against the observation numbers in the data
#  - id.n argument here works to label that number of the most extreme observations individually for each measure plotted.
# ------------------------------------------------------------------------------

car::influenceIndexPlot(M1, vars = c("Cook", "studentized", "hat"), id.n = 4)


