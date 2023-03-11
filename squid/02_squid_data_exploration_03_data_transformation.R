# setwd("//media//kswada//MyFiles//R//squid")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------

Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


dim(Squid)


car::some(Squid)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "Testisweight"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ Testisweight, data = Squid)



# transforming for symmetry
car::symbox(~ Testisweight, data = Squid)


# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.
p1 <- car::powerTransform(Testisweight ~ 1, data = Squid, family = "bcPower")

summary(p1)


# should not use lambda = 0.5  (p-value is very small)
car::testTransform(p1, lambda = 0.5)


# should use lambda = 0.33
car::testTransform(p1, lambda = 0.33)




# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "Specimen"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ Specimen, data = Squid)


# transforming for symmetry  -->  nothing good ...
car::symbox(~ Specimen, data = Squid)


# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "lifespan" untransformed is inconsistent with the goal of making the variabel normally distributed.
p1 <- car::powerTransform(Specimen ~ 1, data = Squid, family = "bcPower")

summary(p1)


# should not use lambda = 0.5  (p-value is very small)
car::testTransform(p1, lambda = 0.5)


# should use lambda = -3.96  (p-value is not small)
car::testTransform(p1, lambda = -3.96)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "DML"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ DML, data = Squid)


# transforming for symmetry  --> log transform is good !!
car::symbox(~ DML, data = Squid)


# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "lifespan" untransformed is inconsistent with the goal of making the variabel normally distributed.
p1 <- car::powerTransform(DML ~ 1, data = Squid, family = "bcPower")

summary(p1)


# should not use lambda = 0.5  (p-value is very small)
car::testTransform(p1, lambda = 0.5)


# should use lambda = 0 (log transformation)  (p-value is large)
car::testTransform(p1, lambda = 0)


