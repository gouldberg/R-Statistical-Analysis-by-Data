
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ARMD (Age-Related Macular Degeneration Trial)
# ------------------------------------------------------------------------------


armd.wide <- read.csv("armd.wide.txt", header = T, sep = "\t")


armd0 <- read.csv("armd0.txt", header = T, sep = "\t")



str(armd.wide)


str(armd0)



# ----------
data("armd", package = "nlmeU")


str(armd)




# ------------------------------------------------------------------------------
# Ordinary linear regression
# ------------------------------------------------------------------------------


lm1.form <- formula(visual ~ -1 + visual0 + time.f + treat.f : time.f)


lm6.1 <- lm(lm1.form, data = armd)



summary(lm6.1)




# ----------
( tT <- coef(summary(lm6.1)) )


rownames(tT) <- abbreviate(rownames(tT))


printCoefmat(tT, P.values = TRUE)



summary(lm6.1)$sigma




# ------------------------------------------------------------------------------
# Sequential-approach F-tests
# ------------------------------------------------------------------------------

anova(lm6.1)




# ------------------------------------------------------------------------------
# model diagnostics
# ------------------------------------------------------------------------------


car::residualPlots(lm6.1)



# -->
# Note that the variance of residuals are larger for the larger time.f


car::densityPlot(resid(lm6.1))




# ----------
par(mfrow = c(2,2))

plot(lm6.1)





# ------------------------------------------------------------------------------
# ordinary regression by gls()
# ------------------------------------------------------------------------------


library(nlme)


fm6.1 <- gls(lm1.form, data = armd)


summary(fm6.1)



# ----------
# 95% CI for beta and sigma

intervals(fm6.1)





