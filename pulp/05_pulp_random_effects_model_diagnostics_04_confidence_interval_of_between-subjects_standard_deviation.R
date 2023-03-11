setwd("//media//kswada//MyFiles//R//pulp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pulp
# ------------------------------------------------------------------------------

data("pulp", package = "faraway")

str(pulp)

car::some(pulp)



# ----------
mmod <- lmer(bright ~ 1 + (1 | operator), data = pulp, REML = TRUE)


mod_obj <- mmod



# ------------------------------------------------------------------------------
# Parametric Bootstrap to construct confidence intervals for parameters
# ------------------------------------------------------------------------------

VarCorr(mod_obj)


as.data.frame(VarCorr(mod_obj))



# ----------
bsd <- numeric(1000)

for(i in 1:1000){
  y <- unlist(simulate(mod_obj))

  # refit function changes only the response in a model we have already fit.
  bmod <- refit(mod_obj, y)
  bsd[i] <- as.data.frame(VarCorr(bmod))$sdcor[1]
}


bsd



# ----------
# The 95% bootstrap confidence interval for sigma(alpha)
quantile(bsd, c(0.025, 0.500, 0.975))



# ----------
# compared with aov model:  quite different
summary(lmod)

sqrt(( 0.447 - 0.106 ) / 5)



# ------------------------------------------------------------------------------
# Parametric Bootstrap to construct confidence intervals for parameters:  by confint
# ------------------------------------------------------------------------------

# Essentially the same result can be obtained more directly using the confint
# note ".sig01"

confint(mod_obj, method = "boot")



# -->
# But in this case, the lower bound is zero.



# ------------------------------------------------------------------------------
# In this example, the random and fixed effect tests gave similar outcomes.
# However, the hypotheses in random and fixed effects are intrinsically different.
# TO generalize somewhat, it is easier to conclude there is an effect in a fixed effects model since the conclusion
# applies only to the levels of the factor used in the experiment,
# while for random effects, the conclusion extends to levels of the factor not considered.
# Since the range of the random effect conclusions is greater, the evidence necessarily has to be stronger.

