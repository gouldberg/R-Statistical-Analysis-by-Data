setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ----------
modp <- glm(intensity ~ length + area * year, data = CodParasites, family = poisson(link = "log"))



# ------------------------------------------------------------------------------
# Confidence interval for the coefficient:  glm model  (profile likelihood base and Wald base)
# ------------------------------------------------------------------------------

summary(modp)



# confint() apply profile likelihood method
# 切片、回帰係数の信頼区間: MASSパッケージの confint()  尤度ベースでの計算であり、サンプル数が小さい場合の精度が高い
confint(modp)



# Wald confidence interval (based on asymptotic normality)
confint.default(modp, level = 0.95)



# ------------------------------------------------------------------------------
# Confidence interval by each variable:  glm model  (profile likelihood base and Wald base)
# ------------------------------------------------------------------------------

# profile likelihood method
exp(confint(modp))



# Wald confidence interval (based on asymptotic normality)
exp(confint.default(modp, level = 0.95))


