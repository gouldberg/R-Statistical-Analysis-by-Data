setwd("//media//kswada//MyFiles//R//attenu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  attenu
# ------------------------------------------------------------------------------

data("attenu")


str(attenu)

dim(attenu)


car::some(attenu)



# ----------
attenu <- na.exclude(attenu)


attenu <- attenu %>% mutate(mag2 = ifelse(mag < 6, 5, ifelse(mag < 7, 6, 7)))



# ----------
mmod <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station) + (1 | event), data = attenu, REML = FALSE)


mod_obj <- mmod



# ------------------------------------------------------------------------------
# Model disgnostics:  Histogram of standardized residuals
# ------------------------------------------------------------------------------

diagd <- fortify(mod_obj)

head(diagd)



# ----------
hist(scale(resid(mod_obj)), freq = FALSE, ylim = c(0, 0.5), xlim = c(-5, 5),
     main = "Histogram of Standardized Residuals", xlab = "Standardized Residuals")

lines(density(scale(resid(mod_obj))))

box()



# ----------
hist(diagd$.scresid)



# ------------------------------------------------------------------------------
# Standardized residuals against fitted value and X, with by group
# ------------------------------------------------------------------------------

plot(mod_obj)


plot(mod_obj, resid(., type = "pearson") ~ fitted(.) | station, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)

plot(mod_obj, resid(., type = "pearson") ~ fitted(.) | event, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)

plot(mod_obj, resid(., type = "pearson") ~ fitted(.) | mag2, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)


plot(mod_obj, resid(., type = "pearson") ~ log(dist) | mag2, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)



# ------------------------------------------------------------------------------
# Distribution of Standardized residuals by group
# ------------------------------------------------------------------------------

plot(mod_obj, station ~ resid(., type = "pearson"))


plot(mod_obj, as.factor(mag2) ~ resid(., type = "pearson"))



# ------------------------------------------------------------------------------
# Model disgnostics:  Q-Q plot
# ------------------------------------------------------------------------------

ggplot(diagd, aes(sample = .resid)) + stat_qq() + facet_grid(~ ifelse(mag < 6, 5, ifelse(mag < 7, 6, 7)))



# -->
# We see that the residuals are almost normally distributed



# ----------
car::qqPlot(resid(mod_obj), groups = ifelse(attenu$mag < 6, 5, ifelse(attenu$mag < 7, 6, 7)))



# ------------------------------------------------------------------------------
# Residuals vs fitted values
# ------------------------------------------------------------------------------

diagd$mag2 <- ifelse(attenu$mag < 6, 5, ifelse(attenu$mag < 7, 6, 7))

ggplot(diagd, aes(x = .fitted, y = .resid)) + geom_point(alpha = 0.3) + geom_hline(yintercept = 0) + facet_grid(~ mag2) +
  xlab("Fitted") + ylab("Residuals")



# -->
# in magnitude 6 and 7 have some increasing trend of residuals
# but we note that there are some nevative residuals at large fitted values
