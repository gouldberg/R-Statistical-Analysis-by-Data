setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ----------
linmod <- lm(involact ~ race + fire + theft + age + log(income), data = chredlin)

# linmod <- lm(involact ~ race + fire + theft + age + income, data = chredlin)




# ------------------------------------------------------------------------------
# model diagnostics:  residuals
# ------------------------------------------------------------------------------

resi <- residuals(linmod)
# resi <- resid(linmod)


summary(resi)



car::densityPlot(resi)

# hist(resi)



# ----------
car::residualPlots(linmod)



# -->
# lack of fit test:  no problem
# Tukey's test for non-additivity (adding the squares fo the fitted values to the model and refitting):  no-problem



# ----------
# residual plot by group

car::residualPlots(linmod, groups = chredlin$side)





# ------------------------------------------------------------------------------
# Identify large residuals
# ------------------------------------------------------------------------------


stand.resid <- rstandard(linmod)


which.max(stand.resid)



# ----------
par(mfrow=c(1,1))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized residuals", type = "h")

abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))





# ------------------------------------------------------------------------------
# check for normality
# ------------------------------------------------------------------------------

qqnorm(resi)
qqline(resi)



car::qqPlot(stand.resid, xlab = "Normal Quantiles", ylab = "Standardized residuals")



# ----------
# by group
car::qqPlot(stand.resid, group = data$type, xlab = "Normal Quantiles", ylab = "Studentized residuals")



