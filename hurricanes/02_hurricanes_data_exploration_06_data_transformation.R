setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\hurricanes")

packages <- c("dplyr")

purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hurricanes
# ------------------------------------------------------------------------------

# data("Hurricanes", package = "rethinking")

data <- read.csv(file = "Hurricanes.txt", header = T, sep = "\t")


dim(data)


str(data)



car::some(data)




# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "deaths"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ deaths, data = data)



# transforming for symmetry  --> 1 / sqrt transformation is good
car::symbox(~ deaths, data = data)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(deaths ~ 1, data = data, family = "bcnPower")

summary(p1)


car::testTransform(p1, lambda = 0.1)



# ---------
tmp <- data %>% mutate(deaths2 = deaths^0.1)


car::densityPlot( ~ deaths2, data = tmp)




# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "damage_norm"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ damage_norm, data = data)



# transforming for symmetry  --> log transformation is good
car::symbox(~ damage_norm, data = data)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(damage_norm ~ 1, data = data, family = "bcnPower")

summary(p1)


car::testTransform(p1, lambda = 0.1)



# ---------
tmp <- data %>% mutate(damage_norm2 = damage_norm^0.1)


car::densityPlot( ~ damage_norm2, data = tmp)


