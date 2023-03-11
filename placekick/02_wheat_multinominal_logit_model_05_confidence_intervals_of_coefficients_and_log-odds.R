setwd("//media//kswada//MyFiles//R//wheat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wheat
# ------------------------------------------------------------------------------

wheat <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter3//wheat.csv")

str(wheat)

car::some(wheat)



# ------------------------------------------------------------------------------
# multinominal logit model
# ------------------------------------------------------------------------------

levels(wheat$type)


library(nnet)

methods(class = multinom)

mod.fit <- multinom(formula = type ~ class + density + hardness + size + weight + moisture, data = wheat)


summary(mod.fit)



# ------------------------------------------------------------------------------
# Confidence intervals for coefficients
# ------------------------------------------------------------------------------

# Unfortunately the confing.multinom() method function that is called by the generic confint() does not produce profile LR intervals.
# The Wald intervals for the odds ratios are given below.

conf.beta <- confint(object = mod.fit, level = 0.95)

round(conf.beta,2)  # Results are stored in a 3D array

conf.beta[2:7,1:2,1]  # C.I.s for beta_2r

conf.beta[2:7,1:2,2]  # C.I.s for beta_3r



# Test the multiplication
c.value * conf.beta[2:7,1:2,1]

c.value[2] * conf.beta[2,1:2,1]



# Another way to do the Wald
# vcov(mod.fit)  # Var^(beta^_1) is the (2,2) element of the matrix
beta.ci <- coef(mod.fit)[1,2] + qnorm(p = c(0.025, 0.975)) * sqrt(vcov(mod.fit)[2,2])
beta.ci



# ------------------------------------------------------------------------------
# Confidence intervals for odds ratios
# ------------------------------------------------------------------------------

# CIs for OR
ci.OR2 <- exp(c.value * conf.beta[2:7,1:2,1])
ci.OR3 <- exp(c.value * conf.beta[2:7,1:2,2])  

round(data.frame(low = ci.OR2[,1], up = ci.OR2[,2]), 2)
round(data.frame(low = 1/ci.OR2[,2], up = 1/ci.OR2[,1]), 2)[c(2,5),]

round(data.frame(low = ci.OR3[,1], up = ci.OR3[,2]), 2)
round(data.frame(low = 1/ci.OR3[,2], up = 1/ci.OR3[,1]), 2)[c(2,3),]



# Another way to do the Wald
round(exp(beta.ci * c.value[1]),2)


# -->
# For the scab vs. healthy comparison, only the density and weight odds ratio confidence intervals do not include 1.
# For the sprout vs. healthy comparison, only the density and hardness oddes ratio confidence intervals do not include 1.
# Odds ratios comparisons can be made for scab vs. sprout too.





