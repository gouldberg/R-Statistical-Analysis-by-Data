# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//wagepan")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


library(plm)



# ------------------------------------------------------------------------------
# data:  wagepan
# ------------------------------------------------------------------------------

wagepan <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wagepan.dta")


str(wagepan)



# ----------
# Generate pdata.frame
wagepan.p <- pdata.frame(wagepan, index = c("nr", "year"))


pdim(wagepan.p)

glimpse(wagepan.p)



# ------------------------------------------------------------------------------
# check variation of variables within individuals
#   - pvar() checks for each variable of a panel if it varies cross-sectionally and over time
# ------------------------------------------------------------------------------

pvar(wagepan.p)



# ------------------------------------------------------------------------------
# Random Effects Estimation
# ------------------------------------------------------------------------------

wagepan.p$yr <- factor(wagepan.p$year)



# ----------
# Estiamte different models
reg.ols <- plm(lwage ~ educ + black + hisp + exper + I(exper^2) + married + union + yr, data = wagepan.p, model = "pooling")

reg.re <- plm(lwage ~ educ + black + hisp + exper + I(exper^2) + married + union + yr, data = wagepan.p, model = "random")

reg.fe <- plm(lwage ~ I(exper^2) + married + union + yr, data = wagepan.p, model = "within")


summary(reg.re)

plot(reg.re)



# ----------
stargazer(reg.ols, reg.re, reg.fe, type = "text", 
          column.labels = c("OLS", "RE", "FE"), keep.stat = c("n", "rsq"), keep = c("ed", "bl", "hi", "exp", "mar", "un"))



# ------------------------------------------------------------------------------
# Housman test
#   - The RE estimatoer needs stronger assumptions to be consistent than the FE estimator. On the other hand, it is more efficient if these assumptions
#     hold and we can include time constant regressors.
# ------------------------------------------------------------------------------

phtest(reg.fe, reg.re)


# -->
# With the p value of 0.0033, the null hypothesis that the RE model is consistent is clearly rejected with sensible significance levels

