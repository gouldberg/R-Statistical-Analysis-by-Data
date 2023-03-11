setwd("//media//kswada//MyFiles//R//wght")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wght
# ------------------------------------------------------------------------------

# This data are the longitudinal mathematics data
# The timing variable is grade (grade at testing) and id is the child identification variable

nlsy_math_long <- read.table("//media//kswada//MyFiles//references//GrowthModeling_StructuralEquationAndMultilevelModelingApproaches//Data//nlsy_math_long_R.dat",
                             na.strings='.')

names(nlsy_math_long) <- c('id', 'female', 'low_birth_weight', 'anti_k1', 'math', 'grade', 'occ', 'age', 'men', 'spring', 'anti')


attach(nlsy_math_long)


str(nlsy_math_long)



# ------------------------------------------------------------------------------
# Plot of the Longitudinal Data
# ------------------------------------------------------------------------------
library(ggplot2)

plot_obs <- ggplot(data=nlsy_math_long, aes(x = grade, y = math, group = id)) + 
  geom_line() + 
  theme_bw() + 
  scale_x_continuous(breaks = 2:8, name = "Grade") +  
  scale_y_continuous(name = "PIAT Mathematics")

print(plot_obs)



# ----------
interaction.plot(grade, id, math, xlab='Grade', ylab='PIAT Mathematics', legend=F)



# ------------------------------------------------------------------------------
# No-Growth model
#
#   - The nlme package has been the primary mixed-effects modeling package available through R and includes both a linear mixed-effects modeling procedure (lme)
#     and a nonlinear mixed-effects modeling procedure (nlme)
#   - The lme4 pakcage is a newer package for fitting linear and nonlinear mixed-effects models (procedures include lmer and nlmer) in R and is able to
#     mixed-effects models to non-normal outcomes (an advantage over nmle)
#   - However, nlme is more flexible when it comes to fitting inherently nonlinear models and its programming is more straightforward.
# ------------------------------------------------------------------------------

library(nlme)


# ----------
ng.math.lme <- lme(math ~ 1, random = ~1 | id, data = nlsy_math_long, method="ML")

summary(ng.math.lme)



# -->
# In nlme, defrees of freedom are calculated based on the amount of longitudinal information or the number of observed datapoints minus the sample size.
# = 2,221 - 932 = 1,289
# nlme takes a regression-like approach where the degrees of freedom are based on the number of datapoints, and an intercept, in the case of the no-growth
# model, is estimated for each person.



# ----------
# We note that nlme does not output significance tests for the random-effects estiamtes.
# However, the significance of the random-effects parameters can be evaluated by using the intervals statement or by fitting a model
# where the random effect is not included in the model and calculating the change in -2 * LL.
# If the change in -2 * LL is significant given the change in the number of estimated parameters, then estimating the random effect improves model fit,
# indicating that  the parameter is significantly different from zero.
# However, when there are more than one random-effect parameter (as in the linear growth model),
# this approach can yield a multiple parameter comparison, which does not provide a separate significance test of each random-effect estimate.

intervals(ng.math.lme)



# ------------------------------------------------------------------------------
# No-Growth model fitting by nlme()
# ------------------------------------------------------------------------------

ng.math.nlme <- nlme(math ~ beta_1 + d_1i,
                     data = nlsy_math_long,
                     fixed = beta_1 ~ 1,
                     random = d_1i ~ 1,
                     group = ~ id,
                     start = c(beta_1 = 40),
                     na.action = "na.omit")                       

summary(ng.math.nlme)



# ----------
# Alternative specification
ng.math.nlme <- nlme(math ~ b_1i,
                     data = nlsy_math_long,
                     fixed = b_1i ~ 1,
                     random = b_1i ~ 1 | id,
                     start = c(b_1i = 40))

summary(ng.math.nlme)
