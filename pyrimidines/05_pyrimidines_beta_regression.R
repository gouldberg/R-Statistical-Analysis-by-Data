setwd("//media//kswada//MyFiles//R//pyrimidines")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pyrimidines
# ------------------------------------------------------------------------------

data("pyrimidines", package = "faraway")

str(pyrimidines)

car::some(pyrimidines)

# pyrimidines <- pyrimidines[-c(6,72),]



# ------------------------------------------------------------------------------
# Beta Regression
#   - The advantage of the Beta-based model is the full distributional model which would allow the construction of full predictive distributions
#     rather than just a point estimate and standard error.
# ------------------------------------------------------------------------------

library(mgcv)


# The default choice of link is the logit funciton.

form <- paste0("activity ~ ", paste0(colnames(pyrimidines)[1:26], collapse = "+"))

modb <- gam(as.formula(form), data = pyrimidines, family = betar())




# ----------
# compare coefficients
dat <- data.frame(coef_qlmod = coef(qlmod), coef_modb = coef(modb))
dat$dif <- dat$coef_qlmod - dat$coef_modb

round(dat, digits = 3)


