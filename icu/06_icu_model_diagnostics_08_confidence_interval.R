setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ----------
# Here we do not apply "rage" and "coma"
# race, coma is represented initially as 3-level factors, but the recoded to binary variables (white, uncons)
# we apply binary variables

ICU2 <- ICU[, -c(4, 20)]

str(ICU2)



# ----------
library(rms)

dd <- datadist(ICU2);  options(datadist = "dd")

icu.lrm2 <- lrm(died ~ age + cancer + systolic + admit + ph + pco + uncons, data = ICU2)

summary(icu.lrm2)



# ------------------------------------------------------------------------------
# Confidence interval for the coefficient:  glm model  (profile likelihood base and Wald base)
# ------------------------------------------------------------------------------

summary(icu.step1)



# confint() apply profile likelihood method
# 切片、回帰係数の信頼区間: MASSパッケージの confint()  尤度ベースでの計算であり、サンプル数が小さい場合の精度が高い
confint(icu.step1)



# Wald confidence interval (based on asymptotic normality)
confint.default(icu.step1, level = 0.95)



# ------------------------------------------------------------------------------
# Confidence interval of odds ratio by each variable:  glm model  (profile likelihood base and Wald base)
# ------------------------------------------------------------------------------

# profile likelihood method
exp(confint(icu.step1))



# Wald confidence interval (based on asymptotic normality)
exp(confint.default(icu.step1, level = 0.95))



# ------------------------------------------------------------------------------
# Confidence interval of odds ratio by each variable:  lrm model  (Wald confidence interval)
# ------------------------------------------------------------------------------

# see "Odds Ratio" in row and "Effect" in column
summary(icu.lrm2)


# odds ratio in log scale
plot(summary(icu.lrm2), log=TRUE, cex = 1.25, col=rgb(.1, .1, .8, alpha=c(.3, .5, .8)))



