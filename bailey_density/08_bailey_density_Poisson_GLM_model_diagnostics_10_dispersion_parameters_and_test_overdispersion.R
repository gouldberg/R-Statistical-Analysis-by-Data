# setwd("//media//kswada//MyFiles//R//bailey_density")
setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ----------
# missing values for the spatial coordinates to be removed
colSums(is.na(DF1))


DF  <- na.exclude(DF1)



# ----------
M1 <- glm(TotAbund ~ MeanDepth, data = DF, family = poisson(link = "log"))

M2 <- glm(TotAbund ~ MeanDepth * fPeriod,  data = DF,  family = poisson)

DF$LogSA <- log(DF$SweptArea)

M3 <- glm(TotAbund ~ MeanDepth * factor(Period) + offset(LogSA), data = DF,  family = poisson)



# ----------
mod_obj <- M3



# ------------------------------------------------------------------------------
# mean vs variance:  check overdispersion
# ------------------------------------------------------------------------------

# For a Poisson distribution, the mean is equal to variance.
# Check the residual variance vs y-hat in log-scale
plot(log(fitted(mod_obj)), log((DF$TotAbund - fitted(mod_obj))^2), xlab=expression(hat(mu)), ylab=expression((y-hat(mu))^2))
# plot(log(rawres^2) ~ log(linpred), xlab = "log(linpred)", ylab = "log(raw resid^2)")

abline(0,1)


# -->
# We see that variance is proportional to, but larger than, the mean  -->  over-dispersion model ??



# ------------------------------------------------------------------------------
# Dispersion parameter
# ------------------------------------------------------------------------------

# residual deviance devided by df.
with(mod_obj, deviance / df.residual)



# ----------
# Poisson distribution has only one parameter and not flexible.
# Estimate the dispersion parameter = chi2 / (n - p)

# Pearson estimate of dispersion
( phi <- sum(residuals(mod_obj, type = "pearson")^2 / mod_obj$df.residual) )


# standard errors of coefficients in this model should be multiplied by, to correct for overdispersion
sqrt(phi)



# -->
# Indicates that standard errors of coefficients in this model should be multipled by 10.5, a 950% increase to correct for overdispersion
# There is substantial overdispersion in the data, possible to the extent that even a negative binomial GLM will not sufficiently adjust for the variation.



# ----------
summary(mod_obj, dispersion=phi)

summary(mod_obj)



# ------------------------------------------------------------------------------
# Testing overdispersion
#   - Tests the null hypothesis of equidispersion in Poisson GLMs against the alternative overdispersion and/or underdispersion
# ------------------------------------------------------------------------------

library(AER)


# null hypothesis of Poisson variation H0: v(y) = mu, 
# against the alternative that the variance has a particular form depending on the mean v(y) = mu + alpha * f(mu)
dispersiontest(mod_obj)



# ----------
# second argument specifies the alternative hypothesis, either as an integer power of mu or a function of the mean
# trafo = 2:  Var[y] = mu + alpha * mu ^ trafo  (this is corresponds to negative binomial model with quadratic cariance function)
# If trafo is specified the test is formulated in terms of the parameter alpha.
dispersiontest(mod_obj, trafo = 2)



