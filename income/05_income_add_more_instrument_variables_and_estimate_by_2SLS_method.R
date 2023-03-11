setwd("//media//kswada//MyFiles//R//income")

packages <- c("dplyr", "AER")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  income
# ------------------------------------------------------------------------------
income8 <- read.table("//media//kswada//MyFiles//references//計量経済学の第一歩//data//csv//8_income.csv", header = TRUE, sep = ",")

str(income8)



# ------------------------------------------------------------------------------
# Apply 2 instrumental variables (payeduc + sibs) for one endogenous variable (yeduc)
#   - Estimated by 2 stage regression
# ------------------------------------------------------------------------------

# 修学年数の操作変数として兄弟姉妹数も加えた2段階最小2乗法でミンサー方程式を推定
ivreg4 <- ivreg(lincome ~ yeduc + exper + exper2 | 
                  exper + exper2 + payeduc + sibs, data = income8)


summary(ivreg4)



# ------------------------------------------------------------------------------
# Add more instrumental variables
# ------------------------------------------------------------------------------
# if you add moyeduc (mother's years of education) as instrument variable
ivreg5 <- ivreg(lincome ~ yeduc + exper + exper2 | payeduc + sibs + moyeduc + exper + exper2, data = income8)

summary(ivreg4)
summary(ivreg5)


# -->
# The coefficient of yeduc is reduced from 0.0699 to 0.0686



# ----------
# but if you add mbirth (month of birth), the coefficient gets larger to 0.071 but not much difference
ivreg6 <- ivreg(lincome ~ yeduc + exper + exper2 | payeduc + sibs + moyeduc + mbirth + exper + exper2, data = income8)

summary(ivreg6)




