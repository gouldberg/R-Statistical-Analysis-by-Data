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
# Fixd Effects Estimation:  Has the Return to Education Changed Over Time ?
#   - The fixed effects (FE) estimator simply estimates the demeaned wage equation using pooled OLS.
#     Instead of applying the within transformation to all variables and running lm(), we can simply use plm() on the original data with the option model = "within".
#   - plm() has the additional advantage that the degrees of freedom are adjusted to the demeaning and the variance-covariance matrix and standard errors are adjusted accordingly.
# ------------------------------------------------------------------------------

mod_fe <- plm(lwage ~ married + union + factor(year) * educ, data = wagepan.p, model = "within")


summary(mod_fe)


