setwd("//media//kswada//MyFiles//R//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------
Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


Squid$fMONTH = factor(Squid$MONTH)



# ----------
vf4 <- varPower(form = ~ DML | fMONTH)
M.gls4 <- gls(Testisweight ~ DML * fMONTH, data = Squid, weights = vf4)
summary(M.gls4)

mod_obj <- M.gls4



# ------------------------------------------------------------------------------
# Confidence interval for the coefficient:  glm model  (profile likelihood base and Wald base)
# ------------------------------------------------------------------------------

summary(mod_obj)



# confint() apply profile likelihood method
# 切片、回帰係数の信頼区間: MASSパッケージの confint()  尤度ベースでの計算であり、サンプル数が小さい場合の精度が高い
confint(mod_obj)



# Wald confidence interval (based on asymptotic normality)
confint.default(mod_obj, level = 0.95)


