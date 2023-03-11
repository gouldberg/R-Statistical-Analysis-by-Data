setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ----------
# mod_obj <- modp.step2
mod_obj <- modp2



# ------------------------------------------------------------------------------
# Confidence interval for the coefficient:  glm model  (profile likelihood base and Wald base)
# ------------------------------------------------------------------------------

summary(mod_obj)



# confint() apply profile likelihood method
# 切片、回帰係数の信頼区間: MASSパッケージの confint()  尤度ベースでの計算であり、サンプル数が小さい場合の精度が高い
confint(mod_obj)



# Wald confidence interval (based on asymptotic normality)
confint.default(mod_obj, level = 0.95)



# ------------------------------------------------------------------------------
# Confidence interval by each variable:  glm model  (profile likelihood base and Wald base)
# ------------------------------------------------------------------------------

# profile likelihood method
exp(confint(mod_obj))



# Wald confidence interval (based on asymptotic normality)
exp(confint.default(mod_obj, level = 0.95))


