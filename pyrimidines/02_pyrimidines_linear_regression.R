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
# linear regression for [0,1] data
# ------------------------------------------------------------------------------

linmod <- lm(activity ~ ., data = pyrimidines)
linmod_g <- glm(activity ~ ., data = pyrimidines, family = gaussian)


summary(linmod)
summary(linmod_g)




# ------------------------------------------------------------------------------
# Goodness of Fit
# ------------------------------------------------------------------------------

# for lm model
summary(linmod)$r.squared



# for glm model
# Chi-square test does not reject (not bad fit)
pchisq(deviance(linmod_g), linmod_g$null.deviance, lower = FALSE)



# ----------
# Nagelkerke R^2  --> nearly equal to lm model R^2
n <- linmod_g$df.null + 1

(1 - exp((linmod_g$dev - linmod_g$null)/n)) / (1 - exp(-linmod_g$null/n))



# ----------
# McFadden's peudo R^2  --> nearly equal to lm model R^2
1 - linmod_g$deviance / linmod_g$null.deviance



# ------------------------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------------------------

# Residuals vs. predicted values
car::scatterplot(residuals(linmod) ~ predict(linmod))


par(mfrow=c(1,2))
plot(linmod, 1)
plot(linmod_g, 1)


# -->
# Wd do not see significant evidence of a violation of the standard assumptions



# ------------------------------------------------------------------------------
# Outliers
# ------------------------------------------------------------------------------

par(mfrow=c(1,2))
halfnorm(hatvalues(linmod))
halfnorm(cooks.distance(linmod))


influence.measures(linmod)



# ----------
res <- car::influencePlot(linmod, id.col = "blue", scale = 8, id.n = 4)

k <- length(coef(linmod))
n <- nrow(pyrimidines)
2 * k / n
3 * k / n

text(x = c(2, 3) * k / n, y = -2.8, c("2k/n", "3k/n"), cex = 1.2)


# ------------------------------------------------------------------------------
# Variance Inflation Factor
# ------------------------------------------------------------------------------

# Huge correlation ...
vif(linmod)

