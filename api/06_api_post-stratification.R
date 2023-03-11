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
# Raking
#   - Post-stratification, raking, and calibration (or GREG estimation) are related ways of using auxiliary information available on the whole population.
#     These methods all involve adjusting the sampling weights so that the known population totals for auxiliary variables are reproduced exactly. 
#   - Raking is a way to approximate post-stratification on a set of variables when only their marginal population distributions are known.
# ------------------------------------------------------------------------------

# First define data frames giving the population totals for schools by type (elementary, middle, high) and
# by whether they met their school-wide growth target.

pop.types <- data.frame(stype = c("E","H","M"), Freq = c(4421, 755, 1018))

pop.schwide <- data.frame(sch.wide = c("No","Yes"), Freq = c(1072, 5122))



# ----------
# Now apply these to rclus1. 
# The first argument to rake is the survey design object. 
# The second is a list of formulas specifying the variables to rake. 
# The third argument is a list of data frames or tables giving the population distributions of each variable. 

rclus1r <- rake(rclus1, list(~ stype, ~ sch.wide), list(pop.types, pop.schwide))

summary(rclus1r)



# ----------
# Raking can also be used when the joint distribution of some variables is known.
# Suppose we know the joint population distribution of school type and the "comparative improvement" target

( pop.imptype <- with(apipop, table(comp.imp, stype)) )


# We can then call rake specifying two margins: sch.wide and the combination of comp.imp and stype

dclus1r <- rake(dclus1, list(~ comp.imp + stype, ~sch.wide), list(pop.imptype, pop.schwide))

summary(dclus1r)


