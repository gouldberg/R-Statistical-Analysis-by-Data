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
# Confidence intervals for coefficients
#   - confint() can be used to construct profile LR confidence intervals for the parameters
# ------------------------------------------------------------------------------

conf.beta <- confint(object = mod.fit.ord, level = 0.95)

conf.beta


# Example with only density
confint(object = mod.fit.ord, parm = "density", level = 0.95)  



# ----------
# Wald
vcov(mod.fit.ord)  # Var^(beta^_1) is the (2,2) element of the matrix
beta.ci <- (-mod.fit.ord$coefficients[2]) + qnorm(p = c(0.025, 0.975))*sqrt(vcov(mod.fit.ord)[2,2])

beta.ci



# ------------------------------------------------------------------------------
# Confidence intervals for log-odds
# ------------------------------------------------------------------------------
c.value * (-conf.beta)
c.value[2] * (-conf.beta[2,])



ci <- exp(c.value * (-conf.beta))
round(data.frame(low = ci[,2], up = ci[,1]), 2)
round(data.frame(low = 1/ci[,1], up = 1/ci[,2]), 2)



# Wald
round(rev(1/exp(beta.ci * c.value[2])),2)

