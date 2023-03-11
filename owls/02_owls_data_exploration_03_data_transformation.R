# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "NegPerChick"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ NegPerChick, data = Owls)



# transforming for symmetry
car::symbox(~ NegPerChick, data = Owls)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(NegPerChick ~ 1, data = Owls, family = "bcPower")

summary(p1)


# should use lambda = 0.25
car::testTransform(p1, lambda = 0.25)



# check density after transformation
tmp <- Owls %>% mutate(tmp = NegPerChick^0.25)
car::densityPlot( ~ tmp, data = tmp)

histogram(tmp$tmp)


# -->
# There are many zeros


# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "SiblingNegotiation"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ SiblingNegotiation, data = Owls)


# transforming for symmetry
car::symbox(~ SiblingNegotiation, data = Owls)


# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "lifespan" untransformed is inconsistent with the goal of making the variabel normally distributed.
p1 <- car::powerTransform(SiblingNegotiation ~ 1, data = Owls, family = "bcPower")

summary(p1)


# should use lambda = 0.25  (p-value is not small)
car::testTransform(p1, lambda = 0.25)



# check density after transformation
tmp <- Owls %>% mutate(tmp = SiblingNegotiation^0.25)
car::densityPlot( ~ tmp, data = tmp)

histogram(tmp$tmp)


# -->
# There are many zeros
