setwd("//media//kswada//MyFiles//R//rugged")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rugged
# ------------------------------------------------------------------------------

data("rugged", package = "rethinking")

d <- rugged

dim(d)

str(d)


car::some(d)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "rgdppc_2000"
# ------------------------------------------------------------------------------

# check density  --> note this has at least 2 modes
car::densityPlot( ~ rgdppc_2000, data = d)



# transforming for symmetry
car::symbox(~ rgdppc_2000, data = d)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(rgdppc_2000 ~ 1, data = d, family = "bcPower")

summary(p1)


# should use lambda = 0
car::testTransform(p1, lambda = 0)



# check density after transformation  -->  We can see 3 modes !!
tmp <- d %>% mutate(tmp = log(rgdppc_2000))
car::densityPlot( ~ tmp, data = tmp)

histogram(tmp$tmp)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "rugged"
# ------------------------------------------------------------------------------

# check density  --> Note that there are some negative values
car::densityPlot( ~ rugged, data = d)


# transforming for symmetry
car::symbox(~ rugged, data = d)


# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,

p1 <- car::powerTransform(rugged ~ 1, data = d, family = "bcnPower")

summary(p1)


# should use lambda = 0.216
car::testTransform(p1, lambda = 0.216)



# check density after transformation
tmp <- d %>% mutate(tmp = rugged^0.216)
car::densityPlot( ~ tmp, data = tmp)

histogram(tmp$tmp)

