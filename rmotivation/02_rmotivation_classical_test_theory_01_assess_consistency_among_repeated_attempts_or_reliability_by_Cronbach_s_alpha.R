setwd("//media//kswada//MyFiles//R//rmotivation")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Rmotivation
# ------------------------------------------------------------------------------

data("Rmotivation", package = "MPsychoR")


str(Rmotivation)



# ----------
# Here we focus on hybrid motivation only and selct 19 dichotomous items associated with this latent variable

( ind <- grep("hyb", colnames(Rmotivation)) )

HybMotivation <- na.omit(Rmotivation[, ind])

car::some(HybMotivation)



# ------------------------------------------------------------------------------
# Cronbach's alpha
#   - The true score model in classical test theory:    X = T + E:   X: observed score in a test,  T:  the true score (unknown),  E: error
#
#   - The concept referring to the tendency toward consistency among repeated attempts to measure the same thing is called reliability.
#     In other words., reliability represents the accuracy with which a test can measure true scores.
#
#     Reliabiity = sigma^2 of T / sigma^2 of X = sigma^2 of T / ( sigma^2 of T + sigma^2 of E )
#
#   - But we do not know sigma^2 of T
#     A trick that makes is possible to get an estimate of reliability is to establish a parallel from of the original test X (with standard deviation of X)
#     Parallel test is a second version of the original test with the same true score and the same error variance.
#     Reliability = sigma^2 of T / sigma^2 of X = cov(XX') / sigma of X * sigma of X'  (X': parallel form of X)
#
#   - from this, sigma of E = sigma of X * sqrt(1 - Reliability)
#
#   - But, in practice, establishing a good parallel test is very hard to come by.
#     Cronbach's idea is that the total score is made up of the k individual item scores, and each item is considered as a single test,
#     conceptually constructing k parallel tests.  (k is number of test item)
#     This allows us to compute a lower bound for the reliability, which is know as Cronbach's alpha
# ------------------------------------------------------------------------------

( k <- ncol(HybMotivation) )

vcmat <- cov(HybMotivation)

sigma2_Xi <- psych::tr(vcmat)

sigma2_X <- sum(vcmat)



# ----------
# Cronbach's alpha: a lower bound for the reliability, conceptually constructin k parallel tests
cronalpha <- k / (k - 1) * (1 - sigma2_Xi / sigma2_X)

round(cronalpha, 2)



# ----------
# standard error of measurement
sqrt(sigma2_X) * sqrt(1 - cronalpha)




# ----------
# using psych package

alpha.hyb <- psych::alpha(HybMotivation)

round(alpha.hyb$total[1], 2)



# -->
# In practice, we aim for an alpha in the are of 0.8 - 0.9.
# Values of alpha > 0.9 may reflect scale burdened by question redundancy and will generally have a lower correlation with external variables
# (which is an indication of low validity)


# In such cases it is suggested to do a closer item inspection in order to determine, if, in fact, wordking redundancy is the problem.
# Note that in the case of a high al@ha value, we cannot conclude that the scale is unidimensional (i.e., all items measure the same construct)




# ------------------------------------------------------------------------------
# Cronbach's alpha computed through ANOVA with main effects for persons and items
# ------------------------------------------------------------------------------

library(reshape2)

Hyb1 <- data.frame(HybMotivation, person = 1:nrow(HybMotivation))

car::some(Hyb1)


Hyblong <- melt(Hyb1, id.vars = c("person"), variable.name = "item")

car::some(Hyblong)



# ----------
Hyblong$person <- as.factor(Hyblong$person)



# ----------
summary(aov(value ~ person + item, data = Hyblong))



# ----------
# Appoximation of Cronbach's alpha can be obtained via the person mean squares and redisual mean squares

round((0.85 - 0.15) / 0.85, 2)

