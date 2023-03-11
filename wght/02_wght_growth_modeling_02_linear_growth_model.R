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
# Linear Growth Model
# ------------------------------------------------------------------------------

library(nlme)


# ----------
# Two is subtracted from grade to center the intercept at the second-grade measurement occasion.
# Thus, parameters associated with the intercept will reflect predicted values in second grade.
grade_c2 <- grade - 2

# use lme() (not nlme())
lg.math.lme <- lme(math ~ grade_c2, random = ~ grade_c2 | id, data = nlsy_math_long, method = "ML")



# ----------
# model comparison
summary(lg.math.lme)
summary(ng.math.lme)

# X^2(3) = 1,555 and p-value < 0.01
anova(ng.math.lme, lg.math.lme)
# anova(lg.math.lme, ng.math.lme)


# -->
# -2 * LL for the linear growth model is 15,937 (= -2 * - 7968.693),
# whereas the -2 * LL for the no-growth model is 17,492 (= -2 * -8745.952)



# -->
# Fixed Effects:
# The mean slope 4.34 is the mean predicted annual change in mathematics scores from 2nd through 8th grade.
# This is the average annual rate of change because a one-unit change in the variable grade is one year.

# Random Effects:
# There is significant variaion in true mathematics scores in 2nd grade because the intervept variance is 64.56 (sd = 8.04)
# and significanty different from zero.
# Additionally there is significant variation in annual true changes in mathematics because the slope variance is estimated to be 0.73 (sd = 0.86)
# and significantly different from zero.

# Correlation:
# There is a nonsignificant negative covariance between true mathematics scores in 2nd grade and true annual changes in mathematics,
# which is estiamted to be -0.18 (r = -0.03).
# Thus, childrens' level of math performance in 2nd grade was unrelated to how quickly they changed from 2nd through 8th grade.

# Residuals:
# Residual variance is estimated to be 36.23 (sd = 6.019) and indicates individual variablity not accounted for the lineare growth model.



# ------------------------------------------------------------------------------
# Linear Growth Model fitting by nlme()
# ------------------------------------------------------------------------------
lg.math.nlme <- nlme(math ~ (beta_1 + d_1i) + (beta_2 + d_2i) * (grade - 2),
                     data = nlsy_math_long,                      
                     fixed = beta_1 + beta_2 ~ 1,                      
                     random = d_1i + d_2i ~ 1,
                     group = ~ id,
                     start = c(beta_1 = 35, beta_2 = 4),
                     na.action = "na.omit")

summary (lg.math.nlme)



# ----------
# Alternative specification
lg.math.nlme <- nlme(math ~ b_1i + b_2i * (grade - 2),
                     data = nlsy_math_long,
                     fixed = b_1i + b_2i ~ 1,
                     random = b_1i + b_2i ~ 1 | id,
                     start = c(b_1i = 35, b_2i = 4))

summary(lg.math.nlme)



# ------------------------------------------------------------------------------
# Predicted trajectories and residuals
# ------------------------------------------------------------------------------
b_1i_hat <- ranef(lg.math.nlme)[,1] + fixef(lg.math.nlme)[1]

b_2i_hat <- ranef(lg.math.nlme)[,2] + fixef(lg.math.nlme)[2] 

child_id <- as.numeric(rownames(ranef(lg.math.nlme)))

estimates <- data.frame(child_id, b_1i_hat, b_2i_hat)

estimates1 <- merge(x = nlsy_math_long, y = estimates,
                   by.x = c('id'), by.y = c('child_id'),
                   all = TRUE)

estimates1$pred <- estimates1$b_1i_hat + estimates1$b_2i_hat * (estimates1$grade - 2) 

estimates1$resid <- estimates1$math - estimates1$pred 



# ----------
# Plotting Predicted Values
plot_pred <- ggplot(data=estimates1, aes(x = grade, y = pred, group = id)) + 
  geom_line() + 
  theme_bw() + 
  scale_x_continuous(breaks = 2:8, name = "Grade") +
  scale_y_continuous(name = "PIAT Mathematics - Predictions")


print(plot_pred)

fitted (lg.math.nlme)



# ----------
# Plotting Residual Values
plot_resid <- ggplot(data=estimates1, aes(x = grade, y = resid, group = id)) + 
  geom_line() + 
  theme_bw() + 
  scale_x_continuous(breaks = 2:8, name = "Grade") +
  scale_y_continuous(name = "PIAT Mathematics - Predictions")


print(plot_resid)

residuals (lg.math.nlme)



# -->
# The residual plot appears to highlight potential model misspecification as the residuals show a trend over time.
# Residuals for the 2nd, 7th, and 8th grades have negative means, whereas residuals for 3rd, 4th, 5th, and 6th grades have positive means.
# This indicates unmodeled nonlinearity in the development of mathematics.
# Also residual plot indicates similar residual variability at each grade, and checking the standard deviation of the residuals at each grade
# agrees with this conclusion as they varied from 4.10 to 5.06

