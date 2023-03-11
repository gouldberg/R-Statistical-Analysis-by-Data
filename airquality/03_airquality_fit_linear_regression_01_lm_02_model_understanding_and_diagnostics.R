setwd("//media//kswada//MyFiles//R//airquality")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  airquality
# ------------------------------------------------------------------------------

data("airquality")


str(airquality)

car::some(airquality)




# ------------------------------------------------------------------------------
# Model understanding for lm model:  goodness of fit
# ------------------------------------------------------------------------------

# Multiple R-squared = 0.6059
# Adjusted R-squared = 0.5948

summary(l1)




# ------------------------------------------------------------------------------
# Model understanding for lm model:  Main effect plot
# ------------------------------------------------------------------------------

library(effects)

plot(predictorEffects(l1))


par(mfrow = c(2,2))
termplot(l1, partial.resid = TRUE, se = T)




# ------------------------------------------------------------------------------
# Model understanding for lm model:  Full model plot
# ------------------------------------------------------------------------------


fit_dat <- cbind(da, pred = predict(l1, type = "response"))


library(ggplot2)


graphics.off()


gg <- ggplot(fit_dat, aes(x = Solar.R, y = pred)) + theme_bw() + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg

gg + facet_grid(~ Month)



# -->
# Solar.R: non linear relationship
# May:  some neative residuals
# August:  some positive residuals
# July:  some negative residuals




# ----------
# Wind and Temp
# gg <- ggplot(fit_dat, aes(x = Wind, y = pred)) + theme_bw() + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg <- ggplot(fit_dat, aes(x = Temp, y = pred)) + theme_bw() + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg

gg + facet_grid(~ Month)




# ------------------------------------------------------------------------------
# Model understanding for lm model:  Confidence interval
# ------------------------------------------------------------------------------


# profile likelihood method
confint(l1, level = 0.95)



# Wald confidence interval (based on asymptotic normality)
confint.default(l1, level = 0.95)




# ------------------------------------------------------------------------------
# Model diagnostics for lm model:  residual plots
# ------------------------------------------------------------------------------

par(mar = c(2,2,2,2))

car::residualPlots(l1, type = "rstandard", quadratic = TRUE, col.quad = gray(0.7), ylim = c(-5,5))


# -->
# Here interestingly, Solar.R is no problem !!!



car::residualPlots(l1, type = "rstandard", groups = da$Month)


# curvature test
ncvTest(l1)




# ------------------------------------------------------------------------------
# Model diagnostics for lm model:  identify large residuals
# ------------------------------------------------------------------------------


stand.resid <- rstandard(l1)


par(mfrow = c(1,1))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized Residuals", type = "h")

abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))



# ----------
# absolute value of standard residual > 2.0 --> 30, 62, 86, 117  (117 is over 3)

which(abs(stand.resid) > 2)

which(abs(stand.resid) > 3)




# ----------
airquality[c(30,62,86,117),]



# -->
# 117:  August 25  small wind and large Ozone
# 62:  July 1
# Note that "48" is not identified here



# ----------
# "48" June 17:  large wind and low Ozone
airquality[48,]




# ------------------------------------------------------------------------------
# Model diagnostics for lm model:  Component-plus-residual plots (partial residual plot)
# ------------------------------------------------------------------------------


car::crPlots(l1, ~ Temp + Wind + Solar.R, smooth = list(span = 0.5), id = TRUE)



# -->
# "48":  large wind but somewhat large Ozone
# "117":  small wind but somewhat large Ozone



op <- par(mfrow = c(1,3))
termplot(l1, partial.resid = TRUE, se = T)
par(op)




# ------------------------------------------------------------------------------
# Model diagnostics for lm model:  Identify influential data by Studentized residuals
# ------------------------------------------------------------------------------


# Studentized residuals

car::influenceIndexPlot(l1, vars = c("studentized"), id.n = 4)



# ----------
# qqplot

car::qqPlot(rstudent(l1), xlab = "Normal Quantiles", ylab = "Studentized  residuals")


# by Month
car::qqPlot(rstudent(l1), group = da$Month, xlab = "Normal Quantiles", ylab = "Studentized  residuals")




# ----------
# half-normal plot

faraway::halfnorm(rstudent(l1))



# ----------
# outlier test

car::outlierTest(l1)




# ------------------------------------------------------------------------------
# Model diagnostics for lm model:  Identify influential data by leverage
# ------------------------------------------------------------------------------


# leverage in halfnorm plot

faraway::halfnorm(hatvalues(l1))



# ----------
# Studentized residuals vs. leverage, showing the value of Cook's distance by the area of the bubble symbol

influencePlot(l1)



par(mfrow = c(1,1), mar = c(5,4,1,1) + .1, cex.lab = 1.2)
res <- car::influencePlot(l1, id.col = "blue", sclae = 8, id.cex = 1.5, id.n = 3)
k <- length(coef(l1))
n <- nrow(da)
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k.n"), cex = 1.2)


idx <- which(rownames(da) %in% rownames(res))

cbind(da[idx, c("Temp", "Solar.R", "Wind", "Month")], res)




# ----------
# collection of index plot

car::influenceIndexPlot(l1, vars = c("Cook", "studentized", "hat"), id = TRUE)



# -->
# Note that "48":  residuals are marginally large and Hat-Values are large.



# ------------------------------------------------------------------------------
# Model diagnostics for lm model:  Identify influential data by dfbetas
#   - Cook's D and DFFITS are overall measure of the total influence that cases have on the regression coefficients and fitted values, respectively
#   - DFBETAs is the simplest measure of influence of observation i, which is the standardized change in the coefficient
#     for each variable due to omitting that observation
# ------------------------------------------------------------------------------


# influence measures for each case
infl <- influence.measures(l1)

summary(infl)




# ----------

dfbetas <- data.frame(infl$infmat[,2:4])

head(dfbetas)


var <- "dfb.Wind"


op <- par(mar = c(5, 5, 1, 1) + .1)

cols <- as.numeric(cut(da$Ozone, 3))


plot(dfbetas[,var], type = "h", col = cols, xlab = "Observation index", ylab = expression(delta * beta[var]), cex.lab = 1.3)
points(dfbetas[,var], col = cols)


big <- abs(dfbetas[,var]) > 0.25
idx <- 1:nrow(dfbetas)
text(idx[big], dfbetas[big, var], label = rownames(dfbetas)[big], cex = 0.9, pos = ifelse(dfbetas[big, var] > 0, 3, 1), xpd = TRUE)
abline(h = c(-0.25, 0, .25), col = "gray")     



# ----------
# scatterplot matrix of DFBETAs
#  - show the pairwise changes in the regression coefficients for the various predictors
#  - joint effect of observations on pairs of coefficients in more complex than is apparent
#    from the univariate vies that appear in the plots along the diagonal

# method = "mahl" to label the most extreme observations
# according to the Mahalanobis distance of each point from the centroid in the plot.

car::scatterplotMatrix(dfbetas, smooth = FALSE, id = TRUE, showLabels = list(method = "mahl", n = 2, ce = 1, location = "lr"),
                      ellipse = TRUE, levels = 0.95, robust = FALSE, diagonal = "histogram", col = gray(0.6))





# -->
# both of "48" and "117" are really BAD fit for all variables ..
