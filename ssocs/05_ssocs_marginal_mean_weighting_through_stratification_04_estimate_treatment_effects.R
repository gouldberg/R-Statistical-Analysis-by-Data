setwd("//media//kswada//MyFiles//R//ssocs")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Estimation of Treatment Effect
# ------------------------------------------------------------------------------

# estimate ATE using marginal mean weighting through stratification to adjust selection bias
surveyDesignATE <- svydesign(ids = ~ 0, strata = ~ STRATA, weights = ~ mmwsATEFinal, data = data.stratification)

surveyDesignATE <- as.svrepdesign(surveyDesignATE, type = c("bootstrap"), replicates = 1000)


# fit a linear regression model to proportions
modelATE <- svyglm(percHarsh ~ treat, surveyDesignATE, family = gaussian())

summary(modelATE)


# fit a logistic model  to proportions with quasibinomial family to allow for overdispersion
modelATE2 <- svyglm(percHarsh~treat, surveyDesignATE, family = quasibinomial())

summary(modelATE2)



# -->
# Neither is statistically significant, indicating that the presence of a full-time security professional does not increase
# the administration of harsh punishment in schools.

# This conclusion is tentative given that covariate balance only met with the less strict criterion of absolute standardized differences below 0.25.



# ----------
# estimate ATT using marginal mean weighting through stratification to adjust selection bias
surveyDesignATT <- svydesign(ids = ~ 0, strata = ~ STRATA, weights = ~ mmwsATTFinal, data = data.stratification)

surveyDesignATT <- as.svrepdesign(surveyDesignATT, type = c("bootstrap"), replicates = 1000)


# fit a linear regression model to proportions
modelATT1 <- svyglm(percHarsh ~ treat, surveyDesignATT, family=gaussian())

summary(modelATT1)


# fit a logistic model  to proportions with quasibinomial family to allow for overdispersion
modelATT2 <- svyglm(percHarsh ~ treat, surveyDesignATT, family = quasibinomial())

summary(modelATT2)
