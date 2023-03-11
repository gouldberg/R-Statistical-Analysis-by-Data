setwd("//media//kswada//MyFiles//R//loblolloy")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 2. GAMs in Practice: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Loblloly
# ------------------------------------------------------------------------------

data(Loblolly, package = "gamair")


# Note that this is "nfnGroupedData" class
str(Loblolly)


head(Loblolly)


# ----------
# Seed was "ordered factor" --> changed to non-ordered factor
Loblolly$Seed <- as.factor(as.character(Loblolly$Seed))


# Without centering, polynomial terms can become highly correlated which can cause numerical difficulties
Loblolly$age <- Loblolly$age - mean(Loblolly$age)



# ------------------------------------------------------------------------------
# model growth with fixed effects of "seed"
# ------------------------------------------------------------------------------

lmod <- lm(height ~ poly(age, 3) + Seed, data = Loblolly)
# lmod <- lm(height ~ age + I(age ^ 2) + I(age ^ 3) + Seed, data = Loblolly)


summary(lmod)



# ----------
anova(lmod)



# ------------------------------------------------------------------------------
# residual plots
# ------------------------------------------------------------------------------

library(car)


residualPlots(lmod)


residualPlots(lmod, term = ~1)



# -->
# Note that the residuals by seed shows heterogeneity



# ----------
qqPlot(lmod)


densityPlot(resid(lmod))



# ------------------------------------------------------------------------------
# Test for non-constant errors
# ------------------------------------------------------------------------------

ncvTest(lmod)



# -->
# Does not reject non-constant variance score test, but p-value is small



# ------------------------------------------------------------------------------
# Main effect plot with partial residuals
# ------------------------------------------------------------------------------

library(effects)

plot(predictorEffects(lmod, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))




# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

lob.fitp <- cbind(Loblolly, height_pred = predict(lmod, type = "response"))

# lob.fitp <- fortify(lmod)

head(lob.fitp)




library(ggplot2)


graphics.off()

gg <- ggplot(lob.fitp, aes(x = age, y = height_pred)) + theme_bw() + geom_point(colour = "black", size = 1.5)
# gg <- ggplot(lob.fitp, aes(x = poly(age, 3), y = .fitted)) + theme_bw() + geom_point(colour = "black", size = 1.5)

gg + facet_wrap(~ Seed) + stat_smooth()


