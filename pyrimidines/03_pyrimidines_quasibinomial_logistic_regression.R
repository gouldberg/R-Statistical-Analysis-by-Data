setwd("//media//kswada//MyFiles//R//pyrimidines")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pyrimidines
# ------------------------------------------------------------------------------

data("pyrimidines", package = "faraway")

str(pyrimidines)

car::some(pyrimidines)

# pyrimidines <- pyrimidines[-c(6,72),]



# ------------------------------------------------------------------------------
# Use quasi-binomial to model the proportion response diretcly
# ------------------------------------------------------------------------------

qlmod <- glm(activity ~ ., family = quasibinomial, pyrimidines)


summary(qlmod)

faraway::sumary(qlmod)


# -->
# dispersion parameter is only 0.016



# ------------------------------------------------------------------------------
# Goodness of Fit
# ------------------------------------------------------------------------------

# Chi-square test does not reject
pchisq(deviance(qlmod), qlmod$null.deviance, lower = FALSE)



# ----------
# Nagelkerke R^2
n <- qlmod$df.null + 1

(1 - exp((qlmod$dev - qlmod$null)/n)) / (1 - exp(-qlmod$null/n))



# ----------
# McFadden's peudo R^2
1 - qlmod$deviance / qlmod$null.deviance



# ------------------------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------------------------

par(mfrow=c(1,2))
halfnorm(hatvalues(qlmod))
halfnorm(cooks.distance(qlmod))



# ----------
par(mfrow=c(1,1))
plot(predict(qlmod), residuals(qlmod, type = "pearson"), xlab = "Linear Predictor", ylab = "Pearson Residuals")



