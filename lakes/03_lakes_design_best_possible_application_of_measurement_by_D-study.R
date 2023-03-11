setwd("//media//kswada//MyFiles//R//lakes")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Lakes
# ------------------------------------------------------------------------------

data("Lakes", package = "MPsychoR")


str(Lakes)


car::some(Lakes)



# ----------
# Here we focus on the physical domain only
# Each of the 194 children in the sample is rated by five raters on three items on his/her self-regulatory ability.
# The ratings are on a scale from 1 to 7.

phydat <- subset(Lakes, subtest == "physical")

phydat$item <- droplevels(phydat$item)

head(phydat)

car::some(phydat)



# ------------------------------------------------------------------------------
# Generalizability Theory:  D-study
#   - D-study makes use of the information provided by the G-study to design the best possible application of the measurement.
#     For instance, we might be interested in whether fewer raters are sufficient or more raters are needed or
#     whether fewer items are sufficient or more items are needed.
# ------------------------------------------------------------------------------

library(gtheory)


# ----------
# G-study

formula <- score ~ (1 | personID) + (1 | raterID) + (1 | item) + (1 | personID:raterID) + (1 | personID:item) + (1 | raterID:item)

gfit <- gstudy(formula = formula, data = phydat)

gfit



# ----------
# D-study variance components
# These variance components are for person mean scores over three items and five raters and result from dividing the G-study variance components
# by the corresponding n

# Here we used the same number of raters and number of items as in the G-study.

dfit <- dstudy(gfit, colname.objects = "personID", colname.scores = "score", data = phydat)

dfit$components



# ----------
# absolute error variance:
#  - simply the difference between a person's observed and universe scores, that is, the sum of all the variance components
#    except the one for the persons
#    = v(i) + v(r) + v(pi) + v(pr) + v(ir) + v(pir, error)
dfit$var.error.abs



# ----------
# absolute standard error of measurement:
#  - can be used to construct a confidence interval for the observed scores
dfit$sem.abs
sqrt(dfit$var.error.abs)


# -->
# If this standard error is too large for our purposes,
# we can change number of raters and number of items accordingly in order to get a samller CI


# ----------
# relative error variance:
#  - the difference between a person's observed deviation score and the universe score
#    = v(i) + v(r) + v(ir)
dfit$var.error.rel



# ----------
# relative standard error of measurement
dfit$sem.rel
sqrt(dfit$var.error.rel)



# ----------
# dependatbility coefficient = v(p) / (v(p) + absolute error variance) --> 0.7744912
dfit$dependability


# generalizability coefficient = v(p) / (v(p) + relative error variance)  --> 0.8298735
dfit$generalizability



# -->
# Those are based on the absolute error variance and relative error variance.
# This generalizability coefficient is analogue of a reliability coefficient in classical test theory.
# Here this is fairly high.

# In the D-study, it can be examined how it would change if we would have, for instance, fewer raters.
# Conversely, if this is too low, the user can study how it can be increased by modifying number of items and number of raters.



