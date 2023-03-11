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
lmod <- aov(bright ~ operator, data = pulp)

mmod <- lmer(bright ~ 1 + (1 | operator), data = pulp, REML = TRUE)

mod_obj <- mmod



# ------------------------------------------------------------------------------
# 95% confidence interval for predicted fixed + random effect
#   - by parametric bootstrap
#   - The predict function for mixed model objects does not compute standard errors or prediction intervals
# ------------------------------------------------------------------------------

group.sd <- as.data.frame(VarCorr(mod_obj))$sdcor[1]

resid.sd <- as.data.frame(VarCorr(mod_obj))$sdcor[2]


pv <- numeric(1000)

for(i in 1:1000){
  y <- unlist(simulate(mod_obj))
  bmod <- refit(mod_obj, y)
  pv[i] <- predict(bmod, re.form = ~0)[1] + rnorm(n = 1, sd = group.sd) + rnorm(n = 1, sd = resid.sd)
}


pv


# ----------
quantile(pv, c(0.025, 0.500, 0.975))



# ------------------------------------------------------------------------------
# 95% prediction interval for predicted fixed + random effect
#   - by parametric bootstrap
# ------------------------------------------------------------------------------

pv2 <- numeric(1000)

for(i in 1:1000){
  
  # we use the option use.u = TRUE indicating that we would simulate new values conditional on the estimated random effects.
  # we need to do this because otherwise we would simulate an entirely new "a" effect in each replication.
  # Instead, we wnat to preserve the originally generated "a" effect.
  y <- unlist(simulate(mod_obj, use.u = TRUE))
  bmod <- refit(mod_obj, y)
  pv2[i] <- predict(bmod, newdata = data.frame(operator = "a")) + rnorm(n = 1, sd = resid.sd)
}


pv2


# ----------
quantile(pv2, c(0.025, 0.500, 0.975))



