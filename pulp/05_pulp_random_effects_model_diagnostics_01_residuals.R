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
# Standardized residuals against fitted value  by group (operator)
# ------------------------------------------------------------------------------

plot(mod_obj)


plot(mod_obj, resid(., type = "pearson") ~ fitted(.) | operator, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)




# ------------------------------------------------------------------------------
# Distribution of Standardized residuals by group (operator)
# ------------------------------------------------------------------------------

plot(mod_obj, operator ~ resid(., type = "pearson"))




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




# ------------------------------------------------------------------------------
# Model disgnostics:  Check the assumption of normality distributed random effects
# ------------------------------------------------------------------------------

qqnorm(ranef(mod_obj)$operator[[1]], main = "Operator effects")

qqline(ranef(mod_obj)$operator[[1]])




