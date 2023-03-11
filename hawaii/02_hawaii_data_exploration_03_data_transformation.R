# setwd("//media//kswada//MyFiles//R//hawaii")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//hawaii")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hawaii
# ------------------------------------------------------------------------------

Hawaii <- read.table(file = "Hawaii.txt", header = TRUE)


str(Hawaii)


dim(Hawaii)


car::some(Hawaii)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "Rainfall"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ Rainfall, data = Hawaii)



# transforming for symmetry
car::symbox(~ Rainfall, data = Hawaii)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(Rainfall ~ 1, data = Hawaii, family = "bcPower")

summary(p1)


# should use lambda = -0.1615
car::testTransform(p1, lambda = -0.1615)



# check density after transformation
tmp <- Hawaii %>% mutate(tmp = Rainfall^(-0.1615))
car::densityPlot( ~ tmp, data = tmp)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "Moorhen.Kauai"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ Moorhen.Kauai, data = Hawaii)


# transforming for symmetry
car::symbox(~ Moorhen.Kauai, data = Hawaii)


# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "lifespan" untransformed is inconsistent with the goal of making the variabel normally distributed.
p1 <- car::powerTransform(Moorhen.Kauai ~ 1, data = Hawaii, family = "bcPower")

summary(p1)


# should use lambda = 0.2708
car::testTransform(p1, lambda = 0.2708)



# check density after transformation
tmp <- Hawaii %>% mutate(tmp = Moorhen.Kauai^0.2708)
car::densityPlot( ~ tmp, data = tmp)

