setwd("//media//kswada//MyFiles//R//flu")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  flu
# ------------------------------------------------------------------------------

data(flu, package = "astsa")

str(flu)

flu



# ------------------------------------------------------------------------------
# switching AR model
# ------------------------------------------------------------------------------


# we model differenced flu

dflu <- diff(flu)


model <- lm(dflu ~ 1)



# ----------
# msmFit:  Fitting Markov Switching Models using the EM algorithm
# msmFit:  Fitting Markov Switching Models using the EM algorithm
# k:  number of regimes
# p:  number of AR coefficients
# sv:  a logical vector indicating which coefficients have switching


library(MSwM)

set.seed(90210)

mod <- msmFit(model, k = 2, p = 2, sw = rep(TRUE, 4))


summary(mod)



plotProb(mod, which = 1)

plotProb(mod, which = 2)

plotProb(mod, which = 3)


