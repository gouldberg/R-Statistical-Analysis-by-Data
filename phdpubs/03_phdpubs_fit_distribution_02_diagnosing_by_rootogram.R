setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
#  - Long (1990, 1987) gave data on the number of publications by 915 doctoral condidates in biochemistry in the last 3 years
#    of their PhD studies.
#    The data set also includes information on:
#      - gender, marital status, number of young children, prestige of the doctoral department, and number of publications by the student's mentor 
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ------------------------------------------------------------------------------
# Rootogram
#
# plot.goodfit() method for "goodfit" objects
# ------------------------------------------------------------------------------

plot(data_fit0, type = "hanging", shade = TRUE, xlab = "Number of Articles", main = "Poisson")



# -->
# Poisson distribution does not fit well at all, since there is a large excess of zero counts
# Most of the counts of 4 or more publications are larger thant the Poisson model predicts  --> zero-inflated models are required.



# ----------
plot(data_fit1, type = "hanging", shade = TRUE, xlab = "Number of Articles", main = "Negative binomial")



# -->
# The fit of the negative binomial model looks much better, except that for 8 or more publications, there is a systematic tendency of
# overfitting for 8 - 10 and underfitting for the observed counts of 12 or more.


# -->
# The difficulty with this simple analysis is not only that it ignores the possible predictors of publishing by these PhD candidates,
# but also, by doing so, it prevents a better, more nuanced explanation of the phenomenon under study.
# --> we need to consider generalized linear models taking potential predictors into account,
# as well as extended zero-inflated models allowing special consideration of zero counts.
