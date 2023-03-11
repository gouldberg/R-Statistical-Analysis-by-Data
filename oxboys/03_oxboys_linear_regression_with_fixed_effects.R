setwd("//media//kswada//MyFiles//R//oxboys")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oxboys
# ------------------------------------------------------------------------------

data("Oxboys", package = "rethinking")

d <- Oxboys

dim(d)

str(d)


car::some(d)



# ------------------------------------------------------------------------------
# model growth with fixed effects of "seed"
# ------------------------------------------------------------------------------

lmod <- lm(height ~ poly(age, 3) + Subject, data = d)
# lmod <- lm(height ~ age + I(age ^ 2) + I(age ^ 3) + Subject, data = d)


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
# Does not reject non-constant variance score test, but p-value is marginal ... (= 0.05)



# ------------------------------------------------------------------------------
# Main effect plot with partial residuals
# ------------------------------------------------------------------------------

library(effects)

plot(predictorEffects(lmod, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))



# ------------------------------------------------------------------------------
# Conditional effects plot
# ------------------------------------------------------------------------------

plot(Effect(c("age", "Subject"), lmod), 
     confint = list(style = "bands"),
     lines = list(multiline = TRUE, lty = c(1,2,3,4,5), col = c("black", gray(0.2), gray(0.4), gray(0.6), gray(0.8))))



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

d.fitp <- cbind(d, height_pred = predict(lmod, type = "response"))

# d.fitp <- fortify(lmod)

head(d.fitp)


library(ggplot2)


graphics.off()

gg <- ggplot(d.fitp, aes(x = age, y = height_pred)) + theme_bw() + geom_point(colour = "black", size = 1.5)
# gg <- ggplot(d.fitp, aes(x = poly(age, 3), y = .fitted)) + theme_bw() + geom_point(colour = "black", size = 1.5)

gg + facet_wrap(~ Subject) + stat_smooth()


