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
# Model understanding for gamma glm model:  goodness of fit
# ------------------------------------------------------------------------------

summary(l1)
summary(l2)


# -->
# gamma glm: Solar.R is highly significant



# ------------------------------------------------------------------------------
# Model understanding for gamma glm model:  Main effect plot
# ------------------------------------------------------------------------------

library(effects)

plot(predictorEffects(l2))


par(mfrow = c(2,2))
termplot(l2, partial.resid = TRUE, se = T)




# ------------------------------------------------------------------------------
# Model understanding for gamma glm model:  Full model plot
# ------------------------------------------------------------------------------


fit_dat <- cbind(da, pred = predict(l1, type = "response"))

fit_dat2 <- cbind(da, pred = predict(l2, type = "response"))


library(ggplot2)


graphics.off()


# In comparison to linear regression,
# the relationship between variable and predicted values are some what changed (looks like non-linear)



# ----------
# Solar
gg <- ggplot(fit_dat, aes(x = Solar.R, y = pred)) + theme_bw() + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg2 <- ggplot(fit_dat2, aes(x = Solar.R, y = pred)) + theme_bw() + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg

gg2



# ----------
# Wind
gg <- ggplot(fit_dat, aes(x = Wind, y = pred)) + theme_bw() + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg2 <- ggplot(fit_dat2, aes(x = Wind, y = pred)) + theme_bw() + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg

gg2



# ----------
# Temp
gg <- ggplot(fit_dat, aes(x = Temp, y = pred)) + theme_bw() + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg2 <- ggplot(fit_dat2, aes(x = Temp, y = pred)) + theme_bw() + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg

gg2




# ------------------------------------------------------------------------------
# Model understanding for gamma glm model:  Confidence interval
# ------------------------------------------------------------------------------


# profile likelihood method
confint(l2, level = 0.95)



# Wald confidence interval (based on asymptotic normality)
confint.default(l2, level = 0.95)




# ------------------------------------------------------------------------------
# Model diagnostics for gamma glm model:  residual plots
# ------------------------------------------------------------------------------

par(mar = c(2,2,2,2))

car::residualPlots(l2, type = "rstandard", quadratic = TRUE, col.quad = gray(0.7), ylim = c(-5,5))


# -->
# Here the cuvature does not ditect !!!


car::residualPlots(l2, type = "rstandard", groups = da$Month)



# curvature test:  not available for glm
# ncvTest(l2)




# ------------------------------------------------------------------------------
# Model diagnostics for gamma glm model:  identify large residuals
# ------------------------------------------------------------------------------


stand.resid <- rstandard(l2)


par(mfrow = c(1,1))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized Residuals", type = "h")

abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))



# ----------
# absolute value of standard residual > 2.0: still 48 and 117 but others !!

which(abs(stand.resid) > 2)

which(abs(stand.resid) > 3)




# ------------------------------------------------------------------------------
# Model diagnostics for gamma glm model:  Component-plus-residual plots (partial residual plot)
# ------------------------------------------------------------------------------


car::crPlots(l2, ~ Temp + Wind + Solar.R, smooth = list(span = 0.5), id = TRUE)




# ------------------------------------------------------------------------------
# Model diagnostics for gamma glm model:  Identify influential data by Studentized residuals
# ------------------------------------------------------------------------------


# Studentized residuals

car::influenceIndexPlot(l2, vars = c("studentized"), id.n = 4)



# ----------
# qqplot

car::qqPlot(rstudent(l2), xlab = "Normal Quantiles", ylab = "Studentized  residuals")


# by Month
car::qqPlot(rstudent(l2), group = da$Month, xlab = "Normal Quantiles", ylab = "Studentized  residuals")




# ----------
# half-normal plot

faraway::halfnorm(rstudent(l2))



# ----------
# outlier test

car::outlierTest(l2)




# ------------------------------------------------------------------------------
# Model diagnostics for gamma glm model:  Identify influential data by leverage
# ------------------------------------------------------------------------------


# leverage in halfnorm plot

faraway::halfnorm(hatvalues(l2))



# ----------
# Studentized residuals vs. leverage, showing the value of Cook's distance by the area of the bubble symbol

influencePlot(l2)



par(mfrow = c(1,1), mar = c(5,4,1,1) + .1, cex.lab = 1.2)
res <- car::influencePlot(l2, id.col = "blue", sclae = 8, id.cex = 1.5, id.n = 3)
k <- length(coef(l2))
n <- nrow(da)
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k.n"), cex = 1.2)


idx <- which(rownames(da) %in% rownames(res))

cbind(da[idx, c("Temp", "Solar.R", "Wind", "Month")], res)




# ----------
# collection of index plot

car::influenceIndexPlot(l1, vars = c("Cook", "studentized", "hat"), id = TRUE)

car::influenceIndexPlot(l2, vars = c("Cook", "studentized", "hat"), id = TRUE)



# -->
# Note that "117" is not recognized as influential (wind is really small)
# but that "48" is still influential (wind is large) 



# ----------
# "9", "21", and "24":  all May
# "21":  ozone is too small
airquality[c(9,21,24),]




# ------------------------------------------------------------------------------
# Model diagnostics for glm model:  Identify influential data by dfbetas
#   - Cook's D and DFFITS are overall measure of the total influence that cases have on the regression coefficients and fitted values, respectively
#   - DFBETAs is the simplest measure of influence of observation i, which is the standardized change in the coefficient
#     for each variable due to omitting that observation
# ------------------------------------------------------------------------------


# influence measures for each case
infl <- influence.measures(l2)

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

infl1 <- influence.measures(l1)
infl2 <- influence.measures(l2)

dfbetas1 <- data.frame(infl1$infmat[,2:4])
dfbetas2 <- data.frame(infl2$infmat[,2:4])


car::scatterplotMatrix(dfbetas1, smooth = FALSE, id = TRUE, showLabels = list(method = "mahl", n = 2, ce = 1, location = "lr"),
                       ellipse = TRUE, levels = 0.95, robust = FALSE, diagonal = "histogram", col = gray(0.3))

car::scatterplotMatrix(dfbetas2, smooth = FALSE, id = TRUE, showLabels = list(method = "mahl", n = 2, ce = 1, location = "lr"),
                       ellipse = TRUE, levels = 0.95, robust = FALSE, diagonal = "histogram", col = gray(0.3))




# -->
# the standard change in coeffs for wind and temperature is really correlated.

