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
# Plot with age at testing as the time metric
# ------------------------------------------------------------------------------
# We use age at testing as the time meric

library(ggplot2)

age_years <- age / 12

plot_obs <- ggplot(data = nlsy_math_long, aes(x = age_years, y = math, group = id)) + 
  geom_line() + 
  theme_bw() + 
  scale_x_continuous(breaks = 6:15, name = "Age") +  
  scale_y_continuous(name = "PIAT Mathematics")


print(plot_obs)


# -->
# Children were assessed at individually varying ages.
# The treatment of continuous time metrics is straightforward in the multilevel modeling framework
# (but not in the structural equation modeling framework)



# ----------
# Age is divided by 12 to scale the slope in years, as opposed to months, and 8 was subtracted to center the intercept at 8 years
age_yr_c8 <- age/12 - 8



# ------------------------------------------------------------------------------
# Linear Growth Model with Age as Time-Metric by lme() 
# ------------------------------------------------------------------------------

library(nlme)


lg.math.age.lme <- lme(math ~ age_yr_c8, random = ~ age_yr_c8 | id, data = nlsy_math_long, method="ML")

summary(lg.math.age.lme)
summary(lg.math.lme)



# ---------- 
AIC(lg.math.age.lme, lg.math.lme)


# -->
# The linear age-based model fits the data better than the linear grade-based model, based on AIC.
# This may be due to the finer measurement of the time metric (especially the differences in time between assessment)
# because age was measure more or less continuously, whereas grade was measured discretely.

# It may be due to age being a more apropriate time metric than grade, or it may be due to other factors.


# ----------
cor(age_yr_c8, grade_c2)


# -->
# However, We note that the correlation between age nad grade was 0.95 (intraclass correlation = 0.56)



# ------------------------------------------------------------------------------
# Linear Growth Model with Age as Time-Metric by nlme()
# ------------------------------------------------------------------------------

lg.math.age.nlme <- nlme(math ~ (beta_1 + d_1i) + (beta_2 + d_2i) * (age / 12 - 8),
                         data = nlsy_math_long,
                         fixed = beta_1 + beta_2 ~ 1,
                         random = d_1i + d_2i~1 | id,
                         start = c(beta_1 = 35, beta_2 = 4))

summary (lg.math.age.nlme)



# ----------
lg.math.age.nlme <- nlme(math ~ b_1i + b_2i * (age / 12 - 8),  
                         data = nlsy_math_long,           
                         fixed = b_1i + b_2i~1,                      
                         random = b_1i + b_2i ~ 1,
                         group = ~id,                      
                         start = c(b_1i = 35, b_2i = 4),
                         na.action = "na.omit")

summary (lg.math.age.nlme)



# ------------------------------------------------------------------------------
# Linear Growth Model with Age as Time-Metric by lmer()
# ------------------------------------------------------------------------------

library(lme4)

lg.math.age.lmer <- lmer(math ~ 1 + age_yr_c8 + (1 + age_yr_c8 | id), data = nlsy_math_long, REML = FALSE)

summary(lg.math.age.lmer)

