setwd("//media//kswada//MyFiles//R//api")

packages <- c("dplyr", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  subsample from the California Academic Performance Index
# ------------------------------------------------------------------------------

data(api)

str(apiclus1)



# ------------------------------------------------------------------------------
# survey data and metadata
# ------------------------------------------------------------------------------

# the one-stage cluster sample from the California Academic Performance Index 
dclus1 <- svydesign(id = ~ dnum, weights = ~ pw, data = apiclus1, fpc = ~ fpc)

summary(dclus1)



# ----------
# Converting this design to use unstratified jackknife (JK1) weights is as simple as

rclus1 <- as.svrepdesign(dclus1)

summary(rclus1)



# ------------------------------------------------------------------------------
# Calibration
#   - Calibration, or GREG estimation, allows continuous as well as discrete auxiliary variables.
#     The method is motivated by regression estimation of a total but can be computed simply by reweighting, using the calibrate() function.
#     The method is described for estimation of a total in Särndal et al Model Assisted Survey Ssampling; 
#     we use the same approach to estimate the total of estimating functions.
# ------------------------------------------------------------------------------


# This example uses the same dclus1 survey design as above. 
# First we calibrate to the school type variable.
# We supply a formula ~stype and must specify population totals for the design matrix produced by this formula.

pop.totals <- c(`(Intercept)` = 6194, stypeH = 755, stypeM = 1018)

dclus1g <- calibrate(dclus1, ~ stype, pop.totals)

summary(dclus1g)



# We could also have given the formula as ~stype-1, with no intercept,
# in which case the population totals would have been simply the three stratum sizes.


# ----------
# Calibration to a single factor variable is equivalent to post-stratification
svymean(~ api00, dclus1)
svymean(~ api00, dclus1g)


svytotal(~ enroll, dclus1)
svytotal(~ enroll, dclus1g)


svytotal(~ stype, dclus1)
svytotal(~ stype, dclus1g)



# ----------
# We can calibrate using the two variables stype and sch.wide, as we did with raking.

dclus1g2 <- calibrate(dclus1, ~ stype + sch.wide, c(pop.totals, sch.wideYes = 5122))

svymean(~ api00, dclus1g)
svymean(~ api00, dclus1g2)



# ------------------------------------------------------------------------------
# Calibration to 1999 API
#    - Finally, we can look at a calibration example with a continuous variable.
#      Imagine that the 1999 API is known for all schools but the 2000 API is known only for the sample.
#      Since there is a high correlation between 1999 and 2000 API (see the graphics example for a scatterplot),
#      calibrating to 1999 API reduces the uncertainty in mean 2000 API considerably.
# ------------------------------------------------------------------------------

# We can calibrate the sampling using the statewide total for the previous year's API
dclus1g3 <- calibrate(dclus1, formula = ~ api99, population = c(6194, 3914069))



# compare summary statistics
svymean(~ api00, dclus1g)
svymean(~ api00, dclus1g3)


# the reduction in uncertainty is less impressive for a subpopulation, as knowing the population total does not give complete information
# about subpopulation total.
svymean(~ api00, subset(dclus1g3, comp.imp=="Yes"))


# the total number of students
svytotal(~ enroll, dclus1g)
svytotal(~ enroll, dclus1g3)


# the number of elementary, middle, and high schools in the state 
# svytotal(~ stype, dclus1g)
# svytotal(~ stype, dclus1g3)


# median, quartiles of the Academic Performance Index in the year 2000
svyquantile(~ api00, dclus1, quantile = c(0.25, 0.5, 0.75), ci = TRUE)
svyquantile(~ api00, dclus1g3, quantile = c(0.25, 0.5, 0.75), ci = TRUE)



# ------------------------------------------------------------------------------
# Using a model with variance proportional to the covariate
#   - it is possible to express the standard ratio estimator of a total as a calibration estimator
# ------------------------------------------------------------------------------

dstrat <- svydesign(id = ~ 1, strata = ~ stype, weights = ~ pw, data = apistrat, fpc = ~ fpc)

ratio <- svyratio(~ api.stu, ~ enroll, dstrat, separate = FALSE)

predict(ratio, total = 3811472)


