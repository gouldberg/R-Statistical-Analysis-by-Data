setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)



# ----------
erings <- with(orings, data.frame(temp = rep(temp, each = 6), damage = as.vector(sapply(orings$damage, function(x) rep(c(0,1), times=c(6-x,x))))))

head(erings)



# ------------------------------------------------------------------------------
# Logistic regression by binary responses vs. binomial and proportion responses
# ------------------------------------------------------------------------------

lmod1 <- glm(damage ~ temp, family = binomial, erings)

lmod2 <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)


summary(lmod1)

summary(lmod2)


# -->
# We see that the parameter estimates, standard errors and the difference in the deviances are the same



# ------------------------------------------------------------------------------
# Confidence interval for the coefficient
# ------------------------------------------------------------------------------

# confint() apply profile likelihood method
confint(lmod1)

confint(lmod2)


# -->
# SAME !!



# ------------------------------------------------------------------------------
# Pearson residual, Standardized residuals
#   - Plots the residuals versus each term in a mean function and versus fitted values
#   - Also computes a curvature test for each of the plots by adding a quadratic term and testing the quadratic to be zero
#   - For linear models, this is Tukey's test for nonadditivity when plotting against fitted values
# ------------------------------------------------------------------------------

residualPlots(lmod1)

residualPlots(lmod2)


# ----------
residualPlots(lmod1, type = "rstandard")

residualPlots(lmod2, type = "rstandard")



# ------------------------------------------------------------------------------
# levearage in halfnorm plot
# ------------------------------------------------------------------------------

halfnorm(hatvalues(lmod1))

halfnorm(hatvalues(lmod2))



# ------------------------------------------------------------------------------
# Goodness of Fit:  The proportion of deviance explained  (Nagelkerke)
# ------------------------------------------------------------------------------

n1 <- lmod1$df.null + 1

n2 <- lmod2$df.null + 1

lmod1$deviance

lmod2$deviance

lmod1$null.deviance

lmod2$null.deviance


# Nagelkerke R^2
(1 - exp((lmod1$deviance - lmod1$null.deviance)/n1)) / (1 - exp(-lmod1$null/n1))

(1 - exp((lmod2$deviance - lmod2$null.deviance)/n2)) / (1 - exp(-lmod2$null/n2))



# -->
# deviance, null.devianc and the goodness of fit is differenet ...


# ------------------------------------------------------------------------------
# Goodness of Fit:  Hosmer - Lemeshow Test
# ------------------------------------------------------------------------------

library(ResourceSelection)


( hl1 <- hoslem.test(lmod1$y, fitted(lmod1), g = 5) )

cbind(hl1$observed, hl1$expected)



( hl2 <- hoslem.test(lmod2$y, fitted(lmod2), g = 5) )

cbind(hl2$observed, hl2$expected)




# ------------------------------------------------------------------------------
# Dispersion parameter
# ------------------------------------------------------------------------------

sum(residuals(lmod2, type = "pearson")^2/lmod2$df.residual)

sum(residuals(lmod1, type = "pearson")^2/lmod1$df.residual)

