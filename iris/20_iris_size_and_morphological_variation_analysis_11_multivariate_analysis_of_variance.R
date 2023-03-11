setwd("//media//kswada//MyFiles//R//iris")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  iris
# ------------------------------------------------------------------------------
data("iris")


str(iris)

car::some(iris)



# ------------------------------------------------------------------------------
# Multivariate analysis of variance (MANOVA)
#   - tests whether groups are similar.
#   - The Mahalanobis distance (considered in linear discriminant analysis) is very colose to the Hotelling t^2 statistic
#     which corresponds to a multivariate generalization of the Student test for hypothesis testing.
# ------------------------------------------------------------------------------

summary(manova(miris ~ as.factor(iris[,5])), test = "Hotelling")


# -->
# The 1st column:  the degrees of freedom of the effect and residual variance-covariance matrices
# The 2nd: Hotelling-Lawley value
# 3rd to 5th: transposition of the multivariate test in terms of an F-test with respective degrees of freedom.
# last cell: the probability for accepting the null hypothesis of equality between groups


# Here, the interspecific variance is larger than the intraspecific one

