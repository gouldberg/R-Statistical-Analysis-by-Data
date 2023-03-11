rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\regression_basics\\davis")


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
# model diagnostics:  residuals
# ------------------------------------------------------------------------------

resi <- residuals(linmod)
# resi <- resid(linmod)


length(resi)



summary(resi)


car::densityPlot(resi)

# hist(resi)



# ----------
car::residualPlots(linmod)


# p-value for "repwt" > 0.05, no problem for fit
# Tukey's test for non-additivity (adding squares of the fitted values to the model and refitting):  no problem



# ----------
# residual plot by group

car::residualPlot(linmod, groups = data2$sex)





# ------------------------------------------------------------------------------
# Identify large residuals
# ------------------------------------------------------------------------------

stand.resid <- rstandard(linmod)


which.max(stand.resid)



# ----------
par(mfrow=c(1,1))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized residuals", type = "h")

abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))



# ----------
data[12,]




# ------------------------------------------------------------------------------
# check for normality
# ------------------------------------------------------------------------------

qqnorm(resi)
qqline(resi)



car::qqPlot(stand.resid, xlab = "Normal Quantiles", ylab = "Standardized residuals")



# ----------
# by group
car::qqPlot(stand.resid, group = data2$sex, xlab = "Normal Quantiles", ylab = "Studentized residuals")



