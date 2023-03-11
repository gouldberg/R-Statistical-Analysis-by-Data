# setwd("//media//kswada//MyFiles//R//bailey_density")
setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ----------
# missing values for the spatial coordinates to be removed
colSums(is.na(DF1))


DF  <- na.exclude(DF1)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "TotAbund"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ TotAbund, data = DF)



# transforming for symmetry
car::symbox(~ TotAbund, data = DF)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(TotAbund ~ 1, data = DF, family = "bcPower")

summary(p1)



# should use lambda = 0.2396
car::testTransform(p1, lambda = 0.2396)




# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "Dens"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ Dens, data = DF)



# transforming for symmetry
car::symbox(~ Dens, data = DF)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "lifespan" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(Dens ~ 1, data = DF, family = "bcPower")

summary(p1)


# should use lambda = 0.2048  (p-value is not small)
car::testTransform(p1, lambda = 0.2048)




# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "SweptArea"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ SweptArea, data = DF)



# transforming for symmetry  --> log transform is good !!
car::symbox(~ SweptArea, data = DF)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "lifespan" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(SweptArea ~ 1, data = DF, family = "bcPower")

summary(p1)


# should use lambda = 0 (log transformation)  (p-value is large)
car::testTransform(p1, lambda = 0)


