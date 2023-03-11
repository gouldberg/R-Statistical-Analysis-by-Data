# ------------------------------------------------------------------------------
# data:  Suicide rates in Germany
# ------------------------------------------------------------------------------
data("Suicide", package = "vcd")

data <- Suicide

data



# ------------------------------------------------------------------------------
# 2-way and larger tables:  classical test of independence
# First, the independence of Hair and Eye is equivalent to the model [Hair][Eye]
# ------------------------------------------------------------------------------

data <- within(data, {
  age_sex <- paste(age.group, toupper(substr(sex, 1, 1)))
})


car::some(data)


( tab <- xtabs(Freq ~ age_sex + method2, data = data) )


AS_M <- MASS::loglm(~ age_sex + method2, data = tab)



# ----------
( tab2 <- xtabs(Freq ~ sex + age.group + method2, data = data) )


ASM <- MASS::loglm(~ sex + age.group + method2, data = tab2)



AS_M

ASM



# -->
# The output includes both the chi-square statistic and the deviance test statistics, 
# both significant, indicating strong lack of fit.


