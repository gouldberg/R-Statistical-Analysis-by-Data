setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "lmtest", "ggplot2", "directlabels", "effects", "car", "nnet")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------
data("TV", package = "vcdExtra")

TV

dim(TV)

str(TV)



# ------------------------------------------------------------------------------
# Convert 3-way table to frequency form
# ------------------------------------------------------------------------------

TV.df <- as.data.frame.table(TV)

car::some(TV.df)



# ----------
levels(TV.df$Network)


# choose "ABC" clearly as baseline category
TV.df$Network <- relevel(TV.df$Network, ref = "ABC")

levels(TV.df$Network)




# ------------------------------------------------------------------------------
# Generalized logit model by nnet::miltinom()
#  - This is used to construct submodels comparing any pair of categories
#  - We have assumed that the effects of Day and Time  are additive on the log odds scale
# ------------------------------------------------------------------------------

# specifying Hess = TRUE saves the Hessian and facilitates calculation of standard errors and hypothesis tests.
# Supply weights argument because each row represents the number of viewers in the Freq variable

tv.multinom <- nnet::multinom(Network ~ Day + Time, data = TV.df, Hess = TRUE, weights = Freq)



# ----------
# The summary() method for "multinom" objects does not calculate test statistics for the estimatedd coeffs by default
# The option Wald = TRUE produces Wald z-test statistics, calculated as z = beta / SE(beta)

summary(tv.multinom)

( stats <- summary(tv.multinom, Wald = TRUE) )



# -->
# The first line in each table pertains to the logit comparing CBS with ABC reference level.
# The second line compares NBC against ABC.



# ----------
# p-values for significance tests (based on standard normal asymptotic approximation)
z <- stats$Wald.ratios

( p <- 2 * (1 - pnorm(abs(z))) )

zapsmall(p)




# ----------
# Assess model terms
car::Anova(tv.multinom)
