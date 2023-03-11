setwd("//media//kswada//MyFiles//R//brains")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brains
# ------------------------------------------------------------------------------

data(brains, package = "gamlss.mx")

str(brains)


car::some(brains)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "body"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ body, data = brains)



# transforming for symmetry
car::symbox(~ body, data = brains)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,

p1 <- car::powerTransform(body ~ 1, data = brains, family = "bcPower")

summary(p1)



# should not use lambda = 0
car::testTransform(p1, lambda = 0)



# ----------
tmp <- brains %>% mutate(body2 = log(body))

car::densityPlot( ~ body2, data = tmp)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "brain"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ brain, data = brains)



# transforming for symmetry
car::symbox(~ brain, data = brains)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,

p1 <- car::powerTransform(brain ~ 1, data = brains, family = "bcPower")

summary(p1)



# should not use lambda = 0
car::testTransform(p1, lambda = 0)



# ----------
tmp <- brains %>% mutate(brain2 = log(brain))

car::densityPlot( ~ brain2, data = tmp)



# ----------
# non-parametric density
h1 <- histSmo(tmp$brain2, plot = TRUE)

h1$hist

h1$density

MASS::truehist(tmp$brain2, col = "grey")

lines(histSmo(tmp$brain2), lty = 1, lwd = 2)

lines(histSmo(tmp$brain2, df = 4), lty = 2, lwd = 2)

legend("topleft", legend = c("local ML", "fixed df"), lty = 1:2, cex = 1)




