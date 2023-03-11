setwd("//media//kswada//MyFiles//R//income")

packages <- c("dplyr", "AER")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  income
# ------------------------------------------------------------------------------
income8 <- read.table("//media//kswada//MyFiles//references//計量経済学の第一歩//data//csv//8_income.csv", header = TRUE, sep = ",")

str(income8)



# ------------------------------------------------------------------------------
# What if applying wrong instrument variable
# ------------------------------------------------------------------------------

# 修学年数を生まれ月に回帰
reg3 <- lm(yeduc ~ mbirth, data = income8)

summary(reg3)


# -->
# the p-value is not statistically significant, not appropriate to apply mbirth as instrument variables


# ----------
# 誤った操作変数を使って教育の収益率を推定
ivreg3 <- ivreg(lincome ~ yeduc | mbirth, data = income8)

summary(ivreg3)


# -->
# The coefficient of yeduc is 0.30 (larger) but not significant


