setwd("//media//kswada//MyFiles//R//lynx")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lynx
# ------------------------------------------------------------------------------

data(lynx, package = "datasets")


str(lynx)


lynx



# ----------
lynxl <- log10(lynx)

dlynxl <- diff(log10(lynx))




# ------------------------------------------------------------------------------
# switching AR model for lynx
# ------------------------------------------------------------------------------


model <- lm(lynx ~ 1)



# ----------
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




# ------------------------------------------------------------------------------
# switching AR model for dlynxl
# ------------------------------------------------------------------------------


model <- lm(dlynxl ~ 1)


mod <- msmFit(model, k = 2, p = 2, sw = rep(TRUE, 4))


summary(mod)



plotProb(mod, which = 1)

plotProb(mod, which = 2)

plotProb(mod, which = 3)


