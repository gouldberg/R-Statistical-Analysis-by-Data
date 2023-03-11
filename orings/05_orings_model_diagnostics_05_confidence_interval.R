setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)


dim(orings)


car::some(orings)



# ------------------------------------------------------------------------------
# Confidence interval for the coefficient
# ------------------------------------------------------------------------------

# confint() apply profile likelihood method
# 切片、回帰係数の信頼区間: MASSパッケージの confint()  尤度ベースでの計算であり、サンプル数が小さい場合の精度が高い
confint(lmod)



# ----------
# 回帰係数の信頼区間: 推定値 ± 1.96 * 標準誤差　の場合
library(broom)
tmp <- tidy(lmod)

tmp$estimate[2] + 1.96 * tmp$std.error[2]

tmp$estimate[2] - 1.96 * tmp$std.error[2]



# ----------
# Wald confidence interval (based on asymptotic normality)
confint.default(lmod, level = 0.95)



# ------------------------------------------------------------------------------
# Confidence interval of odds ratio by each variable:  glm model  (profile likelihood base and Wald base)
# ------------------------------------------------------------------------------

# profile likelihood method
exp(confint(lmod))



# Wald confidence interval (based on asymptotic normality)
exp(confint.default(lmod, level = 0.95))


