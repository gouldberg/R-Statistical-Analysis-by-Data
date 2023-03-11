# setwd("//media//kswada//MyFiles//R//mpls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//mpls")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This examples are based on "Longitudinal Data Analaysis for the Behavioral Sciences Using R"


# ------------------------------------------------------------------------------
# data:  MPLS
#   - Sample data from Minneapolis School District (MPLS)
# ------------------------------------------------------------------------------

MPLS.LS <- read.table("MPLS.LS.txt", header = T)


str(MPLS.LS)


dim(MPLS.LS)


car::some(MPLS.LS)



# ------------------------------------------------------------------------------
# Time as a predictor
#   - y(ij) = beta0(1) + beta1(grade(ij)) + { b0i(1) + b1i(grade(ij)) + error(ij) }
# ------------------------------------------------------------------------------


library(lme4)


# The model specifies a linear change curve for the longitudinal reading scores.
# Individaul variation is allowd for both intercepts and slopes

# REML differs from ML in that it includes a bias adjustment.
# The bias adjustment can be desirable when the sample size is not large.
# However, the bias adjustment is generally undesirable for model comparison

lmer.1 <- lmer(read ~ 1 + grade + (1 + grade | subid), data = MPLS.LS, REML = FALSE)


summary(lmer.1)



# -->
# deviance = -2 * logLik

# The estimated correlation of the random effects has the value cor = -0.74
# The negative correlation indicates that those with lower intercepts tend to have higher slopes and the converse.

# Estimated variance of the random error is listed in the Residual row and has the value = 18.315

# There are 88 rows in the MPLS.LS data, but 8 of these have NA values, therefore, the total number of rows used in the analysis is 80.

# The correlation between the two fixed effects = -0.799.
# This values are useful for detecting multicollinearity among predictor terms.

# Note that LMER output does not provide p-values and SEs for random effects.



# ------------------------------------------------------------------------------
# Comparison of LM and LMER Estimates
# ------------------------------------------------------------------------------

lm.1 <- lm(read ~ grade, data = MPLS.LS)


summary(lm.1)



# -->
# the result of ordinaly regression shows that 
# t-value of grade coefficients is only 2.153, p-value is only 0.0344, marginally significant.



# ----------
car::compareCoefs(lm.1, lmer.1)


round(summary(lm.1)$coefficients, 4)

round(summary(lmer.1)$coefficients, 4)



# -->
# the estimates of the fixed effects are similar
# the LMER estimate of the intercept being a bit smaller than the LM estimate,
# and the LMER estimate of the slope being a bit larger than the LM estiamte.

# Statistical theory suggests even closer agreeemnt as the sample size increases.

# Although the fixed effects estimates are similar, the estiamted SEs are quite different.
# The LMER estimates are approximately half as large as the LM estiamtes.
# THe differences in the parameter estimates result in a t-value for the LMER intercept that is approximately two times
# larger than that of the LM intercept.
# The t-value for the LMER slope is approximately 3 times larger than the t-value of the LM slope.




# ------------------------------------------------------------------------------
# Comparison of LM and LMER Estimates
# ------------------------------------------------------------------------------
