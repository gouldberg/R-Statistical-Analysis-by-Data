setwd("//media//kswada//MyFiles//R//attenu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  attenu
# ------------------------------------------------------------------------------

data("attenu")


str(attenu)

dim(attenu)


car::some(attenu)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "dist"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ dist, data = attenu)



# transforming for symmetry
car::symbox(~ dist, data = attenu)


# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.
p1 <- car::powerTransform(dist ~ 1, data = attenu, family = "bcPower")

summary(p1)



# should use lambda = 0.0608
car::testTransform(p1, lambda = 0.0608)




# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "accel"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ accel, data = attenu)


# transforming for symmetry  -->  nothing good ...
car::symbox(~ accel, data = attenu)


# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "lifespan" untransformed is inconsistent with the goal of making the variabel normally distributed.
p1 <- car::powerTransform(accel ~ 1, data = attenu, family = "bcPower")

summary(p1)


# should use lambda = 0.2491
car::testTransform(p1, lambda = 0.2491)




