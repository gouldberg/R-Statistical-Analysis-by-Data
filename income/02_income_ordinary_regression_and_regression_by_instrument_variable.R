setwd("//media//kswada//MyFiles//R//income")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  income
# ------------------------------------------------------------------------------
income8 <- read.table("//media//kswada//MyFiles//references//計量経済学の第一歩//data//csv//8_income.csv", header = TRUE, sep = ",")

str(income8)



# ------------------------------------------------------------------------------
# regress lincome on yeduc
# ------------------------------------------------------------------------------

cor(income8$lincome, income8$yeduc)


# ----------
# 年収を修学年数に回帰
reg1 <- lm(lincome ~ yeduc, data = income8)

summary(reg1)


# -->
# estimated coefficient of yeduc is 0.055



# ----------
plot(reg1)




# ------------------------------------------------------------------------------
# Search instrumental variables:
# regress yeduc on payeduc (father's years of education)
#   - yeduc (years of eduction) may correlate with "basic ability" and its endogeneous is suspicious.
# ------------------------------------------------------------------------------

# 本人の修学年数を親の修学年数に回帰
reg2 <- lm(yeduc ~ payeduc, data = income8)

summary(reg2)


# -->
# payeduc (years of education) is correlate with yeduc (years of education)



# ------------------------------------------------------------------------------
# Apply instrument variables = payeduc
# ------------------------------------------------------------------------------
library(AER)

ivreg1 <- ivreg(lincome ~ yeduc | payeduc, data = income8)

summary(ivreg1)


# -->
# estimated coefficient of yeduc is 0.030 and is smaller than the coefficient estimated by ordinary regression (= 0.055)
# Also the p-values is 0.08 and not significant.


# ----------
summary(ivreg1, vcov = sandwich, df = Inf, diagnostics = TRUE)
