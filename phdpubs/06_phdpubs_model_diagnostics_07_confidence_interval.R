setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ----------
modp <- glm(articles ~ ., data = PhdPubs, family = poisson(link = "log"))



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


