setwd("//media//kswada//MyFiles//R//cigarettessw")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  CigarettesSW
# ------------------------------------------------------------------------------


data("CigarettesSW", package = "AER")



dim(CigarettesSW)


str(CigarettesSW)



car::some(CigarettesSW)




# ----------
# generate a subset for the year 1995
c1995 <- subset(CigarettesSW, year == "1995")




# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "packs"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ packs, data = CigarettesSW)



# transforming for symmetry
car::symbox(~ packs, data = CigarettesSW)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(packs ~ 1, data = CigarettesSW, family = "bcPower")

summary(p1)


# should use lambda = 0.5
car::testTransform(p1, lambda = 0.5)



# check density after transformation
tmp <- CigarettesSW %>% mutate(tmp = packs ^ 0.5)
car::densityPlot( ~ tmp, data = tmp)



# -->
# Very Interestingly, the distribution does not change much !!




# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "rprice"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ rprice, data = CigarettesSW)


# transforming for symmetry
car::symbox(~ rprice, data = CigarettesSW)


# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "lifespan" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(rprice ~ 1, data = CigarettesSW, family = "bcPower")

summary(p1)


# should use lambda = -1.5
car::testTransform(p1, lambda = -1.5)



# check density after transformation
tmp <- CigarettesSW %>% mutate(tmp = rprice ^ (-1.5))
car::densityPlot( ~ tmp, data = tmp)

