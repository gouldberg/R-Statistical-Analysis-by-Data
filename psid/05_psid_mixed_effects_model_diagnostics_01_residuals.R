setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------
data("psid", package = "faraway")

str(psid)

car::some(psid)



# ----------
psid$cyear <- psid$year - 78

mod_obj <- lmer(log(income) ~ cyear * sex + age + educ + (cyear | person), data = psid)




# ------------------------------------------------------------------------------
# Model disgnostics:  Histogram of standardized residuals
# ------------------------------------------------------------------------------

diagd <- fortify(mod_obj)

head(diagd)



# ----------
hist(scale(resid(mod_obj)), freq = FALSE, ylim = c(0, 0.9), xlim = c(-10, 5),
     main = "Histogram of Standardized Residuals", xlab = "Standardized Residuals")

lines(density(scale(resid(mod_obj))))

box()



# ----------
hist(diagd$.scresid)




# ------------------------------------------------------------------------------
# Standardized residuals against fitted value, cyear and educ  by group (sex)
# ------------------------------------------------------------------------------

plot(mod_obj)


plot(mod_obj, resid(., type = "pearson") ~ fitted(.) | sex, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)


plot(mod_obj, resid(., type = "pearson") ~ cyear | sex, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)


plot(mod_obj, resid(., type = "pearson") ~ educ | sex, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)



# ------------------------------------------------------------------------------
# Distribution of Standardized residuals by group (sex, educ)
# ------------------------------------------------------------------------------

plot(mod_obj, sex ~ resid(., type = "pearson"))

plot(mod_obj, educ ~ resid(., type = "pearson"))



# ------------------------------------------------------------------------------
# Model disgnostics:  Q-Q plot
# ------------------------------------------------------------------------------

ggplot(diagd, aes(sample = .resid)) + stat_qq() + facet_grid(~sex)



# -->
# We see that the residuals are not normally distributed, but have a long tail for the lower incomes.
# We should consider changing the log transformation on the response.
# Furthermore, we see that there is greater variance in the female incomes.
# This suggests a modification to the model.


# ----------
car::qqPlot(resid(mod_obj), groups = psid$sex)



# ------------------------------------------------------------------------------
# Residuals vs fitted values
# ------------------------------------------------------------------------------

# We have broken education into 3 levels: less than high school, high school or more than high school.
diagd$edulevel <- cut(psid$educ, c(0, 8.5, 12.5, 20), labels = c("lessHS", "HS", "moreHS"))

ggplot(diagd, aes(x = .fitted, y = .resid)) + geom_point(alpha = 0.3) + geom_hline(yintercept = 0) + facet_grid(~ edulevel) + xlab("Fitted") + ylab("Residuals")



# -->
# Again we can see evidence that a different response transformation should be considered.
