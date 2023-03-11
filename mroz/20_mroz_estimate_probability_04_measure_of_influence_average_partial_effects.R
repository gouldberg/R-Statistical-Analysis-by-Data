setwd("//media//kswada//MyFiles//R//mroz")

packages <- c("dplyr", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mroz
# ------------------------------------------------------------------------------

library(foreign)


# This data has "inlf" variable
mroz <- read.dta("http://fmwww.bc.edu//ec-p//data//wooldridge//mroz.dta")

str(mroz)

names(mroz)


# oursample <- subset(mroz, !is.na(wage))



# ------------------------------------------------------------------------------
# Measure of influence of each variable:  Average Partial Effects (APE)
# ------------------------------------------------------------------------------

# calculation of linear index at individual values

( xb.log <- predict(logitres) )

( xb.prob <- predict(probitres) )



# ----------
# APE factors

factor.log <- mean( dlogis(xb.log) )

factor.prob <- mean( dnorm(xb.prob) )

cbind(factor.log, factor.prob)



# ----------
# average partial effects = beta * factor

APE.lin <- coef(linprob) * 1

APE.log <- coef(logitres) * factor.log

APE.prob <- coef(probitres) * factor.prob

cbind(APE.lin, APE.log, APE.prob)
