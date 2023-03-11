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

names(nlsy_math_long) <- c('id', 'female', 'lb_wght', 'anti_k1', 'math', 'grade', 'occ', 'age', 'men', 'spring', 'anti')


attach(nlsy_math_long)


str(nlsy_math_long)



# ------------------------------------------------------------------------------
# Linear Growth Model with Time-Invariant Covariates
#   - The low-birthweight:  dichotomous variable coded 0 if the child had a birthweight greater than 5.5 pounds and coded 1 if the child's birthweight was less than 5.5 pounds.
#   - The antisocial behaviour rating:  continuous, and has values ranging from 0 to 8.
#
#   - The research questions examined are:
#       (1) Do low-birthweight children have different intercepts and slopes of mathematcis achievement from normal-birthweight children ?
#       (2) Is antisocial behavior related to the intercept and slope of mathematics achievement ?
# ------------------------------------------------------------------------------
grade_c2 <- grade - 2

summary(lb_wght)

summary(anti_k1)



# ----------
lg.math.lme <- lme(math ~ grade_c2 + anti_k1 + lb_wght + grade_c2 * anti_k1 + grade_c2 * lb_wght, random = ~ grade_c2 |id, data = nlsy_math_long, method="ML")

summary(lg.math.lme)

#Linear mixed-effects model fit by maximum likelihood
