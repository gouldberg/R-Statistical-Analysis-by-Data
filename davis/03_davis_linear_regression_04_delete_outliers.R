rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\regression_basics")



# ------------------------------------------------------------------------------
# data:  Davis
# ------------------------------------------------------------------------------

data <- read.csv("Davis.txt", header = T, sep = "\t")


str(data)


car::some(data)


data2 <- na.exclude(data)



# ----------
linmod <- lm(weight ~ repwt, data = data2)





# ------------------------------------------------------------------------------
# deleting record 12
# ------------------------------------------------------------------------------


linmod2 <- update(linmod, subset = -12)


summary(linmod2)




# -->
# note that Multiple R-squared increased to 0.9724



# ----------
car::residualPlot(linmod2)

residualPlot(linmod2, groups = data2$sex)




# ------------------------------------------------------------------------------
# compare coefficient (compare model)
# ------------------------------------------------------------------------------

car::compareCoefs(linmod, linmod2)




# ------------------------------------------------------------------------------
# Identify large residuals
# ------------------------------------------------------------------------------

stand.resid2 <- rstandard(linmod2)


which.max(stand.resid2)



# ----------
par(mfrow=c(1,1))

plot(stand.resid2, ylim = c(min(-3, stand.resid2), max(3, stand.resid2)), main = "Standardized residuals", type = "h")

abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))



# ----------
data[12,]




# ------------------------------------------------------------------------------
# check for normality
# ------------------------------------------------------------------------------

qqnorm(resid(linmod2))
qqline(resid(linmod2))



car::qqPlot(stand.resid2, xlab = "Normal Quantiles", ylab = "Standardized residuals")



# ----------
# by group
car::qqPlot(stand.resid2, group = data2$sex, xlab = "Normal Quantiles", ylab = "Studentized residuals")




# ------------------------------------------------------------------------------
# model diagnostics:  confidence interval
# ------------------------------------------------------------------------------

car::Confint(linmod2)





# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


# plot main effets of each variable

plot(predictorEffects(linmod2))

predictorEffects(linmod2)



# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve

plot(predictorEffects(linmod2, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))





# ------------------------------------------------------------------------------
# predicted and confidential interval
# ------------------------------------------------------------------------------

pred <- predict(linmod2, data = data2[-12,], se = TRUE, conf.level = 0.95)

ord <- order(data2[-12,]$repwt)



# ----------
graphics.off()

plot(weight ~ repwt, data = data2[-12,])

abline(linmod2)

lines(pred$fit[ord] + pred$se[ord] ~ repwt[ord], data = data2[-12,], lty = 2, col = "blue")
lines(pred$fit[ord] - pred$se[ord] ~ repwt[ord], data = data2[-12,], lty = 2, col = "blue")


