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
# Full model effects plot
# ------------------------------------------------------------------------------

library(effects)


# plot each high-order term

plot(allEffects(probitres, grid = TRUE))


