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
# Estimate statistics in a subpopulation with a complex survey sample
#   - This requires some care, and the resulting standard errors do not depend only on data in the subpopulation.
#      The survey package takes care of all these details transparently, so it is valid simply to take a subset of a survey design object.
# ------------------------------------------------------------------------------

# We want to estimate the total number of students in schools that met their "school-wide growth" and "comparable improvement" targets 
svytotal(~ enroll, subset(dclus1, sch.wide == "Yes" & comp.imp == "Yes"))

svytotal(~ enroll, subset(rclus1, sch.wide == "Yes" & comp.imp == "Yes"))



# ----------
# estimates in a subpopulations
svyby(~ api99, ~ stype, dclus1, svymean)

svyby(~ api99, ~ stype, dclus1, svyquantile, quantiles = 0.5)


# Here we have two subpopulation variables that jointly define six subpopulations,
# and the 1999 and 2000 API means, together with their standard errors, are reported in each subpopulation.
svyby(~ api99 + api00, ~ stype + sch.wide, dclus1, svymean)



# ------------------------------------------------------------------------------
# Tables of summary statistics
# ------------------------------------------------------------------------------
# school type (elementary, middle, high) by whether the school met its "comparable improvement" target.

a <- svymean( ~ interaction(stype, comp.imp), design = dclus1)

a



# ----------
# The ftable function reshapes output like this into a flattened table.
# We specify the variable names and the labels for each level in the rownames argument: 
b <- ftable(a, rownames = list(stype = c("E", "H", "M"), comp.imp = c("No", "Yes")))

round(100 * b, 1)



# ----------
# The second example deals with a table of means produced by svyby.
# This is more straightforward, since svyby already knows the variable names and levels.
# First we estimate the mean of 1999 and 2000 API by school type and comparable improvement target

# a <- svyby(~ api99 + api00, ~ stype + sch.wide, rclus1, svymean, keep.var=TRUE)
a <- svyby(~ api99 + api00, ~ stype + sch.wide, rclus1, svymean)
a

ftable(a)

