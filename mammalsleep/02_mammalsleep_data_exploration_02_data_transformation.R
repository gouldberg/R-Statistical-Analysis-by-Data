setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


dim(mammalsleep)


car::some(mammalsleep)



# ----------
# Calculate dream / sleep
mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "body"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ body, data = mammalsleep)


# transforming for symmetry  --> log transform is good !!
car::symbox(~ body, data = mammalsleep)


# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.
p1 <- car::powerTransform(body ~ 1, data = mammalsleep, family = "bcPower")

summary(p1)


# should not use lambda = 0.5  (p-value is very small)
car::testTransform(p1, lambda = 0.5)




# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "lifespan"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ lifespan, data = mammalsleep)


# transforming for symmetry  --> log transform is good !!
car::symbox(~ lifespan, data = mammalsleep)


# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "lifespan" untransformed is inconsistent with the goal of making the variabel normally distributed.
p1 <- car::powerTransform(lifespan ~ 1, data = mammalsleep, family = "bcPower")

summary(p1)


# should not use lambda = 0.5  (p-value is very small)
car::testTransform(p1, lambda = 0.5)



