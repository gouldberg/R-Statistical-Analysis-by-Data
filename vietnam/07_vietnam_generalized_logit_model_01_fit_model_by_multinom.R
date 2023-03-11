setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)

str(Vietnam)


car::some(Vietnam)



# ----------
levels(Vietnam$response)


# choose "A" clearly as baseline category
Vietnam$response <- relevel(Vietnam$response, ref = "A")

levels(Vietnam$response)




# ------------------------------------------------------------------------------
# Generalized logit model by nnet::miltinom()
#  - This is used to construct submodels comparing any pair of categories
#  - We have assumed that the effects of Day and Time  are additive on the log odds scale
# ------------------------------------------------------------------------------

# specifying Hess = TRUE saves the Hessian and facilitates calculation of standard errors and hypothesis tests.
# Supply weights argument because each row represents the number of viewers in the Freq variable

viet.multinom <- nnet::multinom(response ~ sex + year, data = Vietnam, Hess = TRUE, weights = Freq)


# with interactions
viet.multinom2 <- nnet::multinom(response ~ sex * year, data = Vietnam, Hess = TRUE, weights = Freq)



# ----------
# The summary() method for "multinom" objects does not calculate test statistics for the estimatedd coeffs by default
# The option Wald = TRUE produces Wald z-test statistics, calculated as z = beta / SE(beta)

summary(viet.multinom)
summary(viet.multinom2)

( stats <- summary(viet.multinom, Wald = TRUE) )
( stats2 <- summary(viet.multinom2, Wald = TRUE) )




# ----------
# p-values for significance tests (based on standard normal asymptotic approximation)
z <- stats$Wald.ratios

( p <- 2 * (1 - pnorm(abs(z))) )

zapsmall(p)



# ----------
z2 <- stats2$Wald.ratios

( p2 <- 2 * (1 - pnorm(abs(z2))) )

zapsmall(p2)



# ----------
# Assess the model terms
car::Anova(viet.multinom)

car::Anova(viet.multinom2)


# -->
# intercation is marginally significant...
