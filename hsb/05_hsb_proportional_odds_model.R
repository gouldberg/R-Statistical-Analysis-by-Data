setwd("//media//kswada//MyFiles//R//hsb")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hsb
# ------------------------------------------------------------------------------

data("hsb", package = "faraway")


str(hsb)


head(hsb)



# ------------------------------------------------------------------------------
# Proportional Odds Model
# ------------------------------------------------------------------------------

hsb$prog2 <- factor(hsb$prog, levels = c("vocation", "general", "academic"), ordered = TRUE)


library(MASS)


pomod <- polr(prog2 ~ gender + race + ses + schtyp + read + write + math + science + socst, data = hsb)


pomod



# ----------
c(deviance(mmod), mmod$edf)

c(deviance(pomod), pomod$edf)


pchisq(deviance(pomod) - deviance(mmod), mmod$edf - pomod$edf, lower = FALSE)



# -->
# The proportiona odds model uses fewer parameters, but does not fit quite as well.



# ------------------------------------------------------------------------------
# Model selection by AIC-based
# ------------------------------------------------------------------------------

pomodi <- step(pomod, direction = "both")


pomodi



# ----------
# Likelihood ratio test to compare the models
c(deviance(mmodi), mmodi$edf)


c(deviance(pomodi), pomodi$edf)


pchisq(deviance(pomodi) - deviance(mmodi), mmodi$edf - pomodi$edf, lower = FALSE)



# -->
# We see that the simplification to just income is justifiable.

