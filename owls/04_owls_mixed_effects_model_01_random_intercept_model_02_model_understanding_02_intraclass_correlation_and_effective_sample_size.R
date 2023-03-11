# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)



# ----------
library(nlme)

Form <- formula(LogNeg ~ SexParent * FoodTreatment + SexParent * ArrivalTime)

M1.lme <- lme(Form, random = ~ 1 | Nest, method = "REML", data = Owls)



# ----------
mod_obj <- M1.lme



# ------------------------------------------------------------------------------
# model understanding:  interclass correlation
# ------------------------------------------------------------------------------

summary(mod_obj)


# StdDev for the random intercept 
d <- 0.093


# StdDev for the residual
sigma <- 0.232


d^2

d^2 + sigma^2



# induced correlation (or interclass correlation):  The correlation between two observations from the same Nest
( rho <- d^2 / (d^2 + sigma^2) )


# -->
# induced correlation is 0.138, which is relatively low.

# BUT THIS IS NOT CORRECT, SINCE THE SAMPLE OF EACH NEST IS DIFFERENT ..


