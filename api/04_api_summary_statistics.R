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
# summary statistics
# ------------------------------------------------------------------------------

# mean of the Academic Performance Index in the year 2000
svymean(~ api00, dclus1)
svymean(~ api00, rclus1)


# The population variance is estimated with svyvar
svyvar(~ api00, dclus1)
svyvar(~ api00, rclus1)


# median, quartiles of the Academic Performance Index in the year 2000
svyquantile(~ api00, dclus1, quantile = c(0.25, 0.5, 0.75), ci = TRUE)
svyquantile(~ api00, rclus1, quantile = c(0.25, 0.5, 0.75), ci = TRUE)


# the number of elementary, middle, and high schools in the state 
svytotal(~ stype, dclus1)
svytotal(~ stype, rclus1)


# the total number of students
svytotal(~ enroll, dclus1)
svytotal(~ enroll, rclus1)


# proportion who took test
svyratio(~ api.stu, ~ enroll, dclus1)
svyratio(~ api.stu, ~ enroll, rclus1)



# ------------------------------------------------------------------------------
# summary statitstic: more advanced
# ------------------------------------------------------------------------------

# we can ask for means of more than one variable at a time
# Here we have the means of 1999 and 2000 API and school type (elementary, middle, high).
# Note that for the factor variable stype the proportion in each category is reported
svymean(~ api00 + api99 + stype, dclus1)

# here the standard errors are computed from the jackknife replicate weights and are slightly different.
svymean(~ api00 + api99 + stype, rclus1)



# ----------
# Totals are estimated with svytotal.
# Here we estimate the total number of students enrolled, and the total number of schools by type, across the California population. 
svytotal(~ enroll + stype, dclus1)
svytotal(~ enroll + stype, rclus1)



# ----------
# The functions for totals and means can also report the design effect, with the option deff=TRUE
svytotal(~ enroll + stype, dclus1, deff = TRUE)



# ----------
# The proportion of high school students who took the test
# In this example we estimate the proportion of students who took the API test from the number who took the test and the number enrolled. 
svyratio(~ api.stu, ~ enroll, dclus1)
svyratio(~ api.stu, ~ enroll, rclus1)


svyratio(~ api.stu, ~ enroll, design = subset(dclus1, stype == "H"))


# -->
# warnings referred to in the output occurred because several school districts have only one high school sampled, 
# making second stage standard error estimation unreliable.



# ----------
# Specifying a large number of variables is made easier by the make.formula function
vars <- names(apiclus1)[c(12:13, 16:23, 27:37)]
svymean(make.formula(vars), dclus1, na.rm = TRUE)



# ----------
# Summary statistics for subsets can also be computed with svyby
# Here wecompute the average proportion of “English language learners” and of students eligible for subsidized school meals
# for elementary, middle, and high schools
svyby(~ ell + meals, ~ stype, design = dclus1, svymean)

