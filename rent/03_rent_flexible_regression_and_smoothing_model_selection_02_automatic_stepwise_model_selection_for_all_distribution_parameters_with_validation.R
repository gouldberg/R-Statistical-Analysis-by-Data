setwd("//media//kswada//MyFiles//R//rent")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rent
# ------------------------------------------------------------------------------
data("rent", package = "gamlss.data")


str(rent)

car::some(rent)



# ------------------------------------------------------------------------------
# Automatic model selection for all distribution parameters by stepTGDAll.A() with validation and test data
# ------------------------------------------------------------------------------

set.seed(123)


rand <- sample(2, dim(rent)[1], replace = TRUE, prob = c(0.6, 0.4))



# ----------
# the proportions in the sample
table(rand) / dim(rent)[1]



# ----------
# training and validation data
oldrent <- rent[rand == 1,]

newrent <- rent[rand == 2,]



# ----------
# null model
v0 <- gamlss(R ~ 1, data = oldrent, family = GA)



# ----------
( nC <- detectCores() )


v5 <- stepTGDAll.A(v0, scope = ~pb(Fl) + pb(A) + H + loc, newdata = newrent, parallel = "snow", ncpus = nC)


v5



# -->
# All four terms are selected for the mu model.
# pb(Fl) is excluded from the sigma model



