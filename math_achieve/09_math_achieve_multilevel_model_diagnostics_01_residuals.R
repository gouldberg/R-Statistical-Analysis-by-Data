setwd("//media//kswada//MyFiles//R//math_achieve")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MathAchieve
# ------------------------------------------------------------------------------

data("MathAchieve", package = "nlme")

dim(MathAchieve)

str(MathAchieve)

car::some(MathAchieve)



# ----------
data("MathAchSchool", package = "nlme")

dim(MathAchSchool)

str(MathAchSchool)

car::some(MathAchSchool)



# ----------
names(MathAchieve) <- tolower(names(MathAchieve))
names(MathAchSchool) <- tolower(names(MathAchSchool))


Temp <- MathAchieve %>% group_by(school) %>% summarize(mean.ses = mean(ses))
Temp <- merge(MathAchSchool, Temp, by = "school")
car::brief(Temp)


# ----------
HSB <- merge(Temp[, c("school", "sector", "mean.ses")], MathAchieve[, c("school", "ses", "mathach")], by = "school")



# ----------
HSB$cses <- with(HSB, ses - mean.ses)
car::brief(HSB)



# ----------
HSB$sector <- factor(HSB$sector, levels = c("Public", "Catholic"))

hsb.lme.1 <- lme(mathach ~ mean.ses * cses + sector * cses,
                 random = ~ cses | school, data = HSB)

hsb.lmer.1 <- lmer(mathach ~ mean.ses * cses + sector * cses + (cses | school), data = HSB)


mod_obj <- hsb.lme.1
# mod_obj <- hsb.lmer.1



# ------------------------------------------------------------------------------
# Model disgnostics:  Histogram of standardized residuals
# ------------------------------------------------------------------------------

library(ggplot2)


# fortify can be applied to lmer object
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
# Standardized residuals against fitted value  by group (sector)
# ------------------------------------------------------------------------------

plot(mod_obj)


plot(mod_obj, resid(., type = "pearson") ~ fitted(.) | sector, abline = 0, lty = 2, col = gray(0.7), cex = 0.3, pch = 20)




# ------------------------------------------------------------------------------
# Distribution of Standardized residuals by group (sector, school)
# ------------------------------------------------------------------------------

plot(mod_obj, sector ~ resid(., type = "pearson"))

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

ggplot(diagd, aes(x = .fitted, y = .resid)) + geom_point(alpha = 0.3) + geom_hline(yintercept = 0) + xlab("Fitted") + ylab("Residuals") + facet_wrap(~ sector)

ggplot(diagd, aes(x = .fitted, y = .scresid)) + geom_point(alpha = 0.3) + geom_hline(yintercept = 0) + xlab("Fitted") + ylab("Residuals")




# ------------------------------------------------------------------------------
# Model disgnostics:  Check the assumption of normality distributed random effects
# ------------------------------------------------------------------------------

qqnorm(ranef(mod_obj)[[1]], main = "School effects")

qqline(ranef(mod_obj)[[1]])



# -->
# We see that there is approximate normality in both cases with some evidence of short tails for the school effects.

