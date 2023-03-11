setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
# ------------------------------------------------------------------------------

data("jsp", package = "faraway")

str(jsp)

car::some(jsp)



# ----------
# We shall take as our response he math test score result from the final year and try to model this as a function of gender,
# social class and the Raven's test score from the first year which math be taken as a measure of ability when entering the school.

jspr <- jsp[jsp$year == 2,]


car::some(jspr)



# ------------------------------------------------------------------------------
# Final model
# ------------------------------------------------------------------------------

# Given that we have decided that gender is not important, we simplify
# We centered the Raven score about its overall mean. This means that we can interpret the social effects as the predicted differences from
# social class one at the mean Raven score.
# If we did not do this, these parameter estimates would represent differences for raven = 0 which is not very useful.

jspr$craven <- jspr$raven - mean(jspr$raven)

mmod_final <- lmer(math ~ craven * social + (1 | school) + (1 | school : class), data = jspr)

summary(mmod_final)



# -->
# We can see that for the scaled entering score, the final math score tends to be lower as social class goes down.
# Note that class 9 here is when the father is absent and class 8 is not necessarily worse than 7, so this factor is not entirely ordinal.
# We also see the most substantial variation at the individual level with smaller amounts of variation at the school and class level.



# ----------
mod_obj <- mmod_final



# ------------------------------------------------------------------------------
# Model disgnostics:  Histogram of standardized residuals
# ------------------------------------------------------------------------------

diagd <- fortify(mod_obj)

head(diagd)



# ----------
hist(scale(resid(mod_obj)), freq = FALSE, ylim = c(0, 0.7), xlim = c(-4, 5),
     main = "Histogram of Standardized Residuals", xlab = "Standardized Residuals")

lines(density(scale(resid(mod_obj))))

box()



# ----------
hist(diagd$.scresid)



# ------------------------------------------------------------------------------
# Standardized residuals against fitted value, cyear and educ  by group (sex)
# ------------------------------------------------------------------------------

plot(mod_obj)


plot(mod_obj, resid(., type = "pearson") ~ fitted(.) | social, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)


plot(mod_obj, resid(., type = "pearson") ~ fitted(.) | school, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)


plot(mod_obj, resid(., type = "pearson") ~ raven | social, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)



# ------------------------------------------------------------------------------
# Distribution of Standardized residuals by group (sex, educ)
# ------------------------------------------------------------------------------

plot(mod_obj, social ~ resid(., type = "pearson"))

plot(mod_obj, school ~ resid(., type = "pearson"))



# ------------------------------------------------------------------------------
# Model disgnostics:  Q-Q plot
# ------------------------------------------------------------------------------

ggplot(diagd, aes(sample = .resid)) + stat_qq()

ggplot(diagd, aes(sample = .scresid)) + stat_qq()



# ----------
qqnorm(scale(resid(mod_obj)))
qqline(scale(resid(mod_obj)))



# ----------
car::qqPlot(resid(mod_obj))



# ------------------------------------------------------------------------------
# Model disgnostics:  Residuals vs. Fitted values
# ------------------------------------------------------------------------------

ggplot(diagd, aes(x = .fitted, y = .resid)) + geom_point(alpha = 0.3) + geom_hline(yintercept = 0) + xlab("Fitted") + ylab("Residuals")

ggplot(diagd, aes(x = .fitted, y = .scresid)) + geom_point(alpha = 0.3) + geom_hline(yintercept = 0) + xlab("Fitted") + ylab("Residuals")



# -->
# We see that the residuals are close to normal, but there is a clear decrease in the variance with an increase in the fitted values.
# This is due to the reduced variation in higher scores.
# We might consider a transformation of the response to remove this effect.



# ------------------------------------------------------------------------------
# Model disgnostics:  Check the assumption of normality distributed random effects
# ------------------------------------------------------------------------------

qqnorm(ranef(mmod)$school[[1]], main = "School effects")
qqline(ranef(mmod)$school[[1]])


qqnorm(ranef(mmod)$"school:class"[[1]], main = "Class effects")
qqline(ranef(mmod)$"school:class"[[1]])


# -->
# We see that there is approximate normality in both cases with some evidence of short tails for the school effects.

