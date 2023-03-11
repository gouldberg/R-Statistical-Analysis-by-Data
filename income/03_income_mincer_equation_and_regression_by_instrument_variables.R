setwd("//media//kswada//MyFiles//R//income")

packages <- c("dplyr", "AER")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  income
# ------------------------------------------------------------------------------
income8 <- read.table("//media//kswada//MyFiles//references//計量経済学の第一歩//data//csv//8_income.csv", header = TRUE, sep = ",")

str(income8)



# ------------------------------------------------------------------------------
# regression by mincer equation
# ------------------------------------------------------------------------------

# ミンサー方程式を重回帰で推定
reg2 <- lm(lincome ~ yeduc + exper + exper2, data = income8)

summary(reg2)


# -->
# coefficient of yeduc (years of education) is 0.084


plot(reg2)



# ------------------------------------------------------------------------------
# regression by mincer equation with instrument variables
# ------------------------------------------------------------------------------

# ミンサー方程式を操作変数法で重回帰
ivreg2 <- ivreg(lincome ~ yeduc + exper + exper2 | 
                  exper + exper2 + payeduc, data = income8)


summary(ivreg2)


# -->
# coefficient of yeduc (years of education) is 0.075 and is smaller than the coefficient estimated by yeduc




