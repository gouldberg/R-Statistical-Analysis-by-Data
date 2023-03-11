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
# Automatic model selection by stepTGD() with validation and test data
#   - stepTGD() behaves similarly to stepGAIC(), but it uses the test global deviance instead of GAIC as selection criterion.
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


v1 <- stepTGD(v0, scope = ~pb(Fl) + pb(A) + H + loc, newdata = newrent, parallel = "snow", ncpus = nC)


v1$anova


summary(v1)


# -->
# all four terms are selected for the mu model.



# ------------------------------------------------------------------------------
# Check the significance of terms by drop1TGD() and add1TGD()
# ------------------------------------------------------------------------------

v2 <- drop1TGD(v1, newdata = newrent, parallel = "snow", ncpus = nC)


v2



# -->
# The test deviance from the full model is 10874 and there is no improvement when we drop each of four terms one at a time.
# We conclude that all terms are needed in the model.



# ----------
v3 <- add1TGD(v0, newdata = newrent, scope = ~pc(Fl) + pb(A) + H + loc, parallel = "snow", ncpus = nC)


v3



# -->
# The null model test deviance is 11242 and adding any one of the four terms improves the test global deviance.


