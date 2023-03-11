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
# Generalizability Theory:  G-study
#   - We consider two facets: items and raters. Note that in case of multiple mesurement occasions, time could be included as their facet.
#   - We fit a random-effects ANOVA model
#       - X(pir) = mu + v(p) + v(i) + v(r) + v(pi) + v(pr) + v(ir) + v(pir,error)
#         p: person,  i: item,  r: raters
#         mu: grand population mean
#         v: main and interaction effects
#         v(pir, error):  a residual effect involving the three-way interaction and all other sources of error not captured by the specification.
#   - The gstudy function in the gtheory package fits a random-effects ANOVA (via lme4) and etracts the variance components.
# ------------------------------------------------------------------------------

library(gtheory)


formula <- score ~ (1 | personID) + (1 | raterID) + (1 | item) + (1 | personID:raterID) + (1 | personID:item) + (1 | raterID:item)

gfit <- gstudy(formula = formula, data = phydat)

gfit


# -->
# We see that most of the variance in the data is explained by the differences in the children (personID) and 
# that there is some amount of unexplained variance left.

# personID:  how much the children differ in their self-regulation
# item: how homogeneous the items are
# raterID: whether som raters are more lenient than others in their scoring
# personID:item: relative differences in relative self-regulation across items, averaged over raters.
# personID:raterID: relative differenes self-regulation across raters, average over raters.
# item:raterID: inconsistency of raters' average ratings of children from one item to the next
# residual: capture three-way interaction plus error (i.3., ths systematic variation due to sources not controlled for in this design and unsystematic variation due to sources that cannot be controlled)









