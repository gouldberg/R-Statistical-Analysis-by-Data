# setwd("//media//kswada//MyFiles//R//hawaii")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//hawaii")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hawaii
# ------------------------------------------------------------------------------

Hawaii <- read.table(file = "Hawaii.txt", header = TRUE)


str(Hawaii)


dim(Hawaii)


car::some(Hawaii)


# ----------
Hawaii$Birds <- sqrt(Hawaii$Moorhen.Kauai)




# ------------------------------------------------------------------------------
# Impose correlation structure:  corAR1
#   - Check the correlation matrix
# ------------------------------------------------------------------------------


# corAR1
# for example value = 0.3

cs1 <- corAR1(value = 0.3, form = ~Year)


# cs1 <- Initialize(cs1, Hawaii)



# ----------
coef(cs1, unconstrained = FALSE)


coef(cs1)


corMatrix(cs1, covariate = 1:4)




# ------------------------------------------------------------------------------
# Impose correlation structure:  corAR1
#   - Suppose rho = 0.5.  The correlation between residuals separated by one unit in time is then 0.5.
#     If the separation is two units in time, the correlation is 0.5^2 = 0.25.
#     Hench, the further away two residuals are separated in time, the lower their correlation.
#     For many ecological examples, this makes sanse.
# ------------------------------------------------------------------------------

library(nlme)


M2 <- gls(Birds ~ Rainfall + Year, na.action = na.omit, data = Hawaii, 
          correlation = corAR1(form = ~ Year))


summary(M2)



# -->
# Phi1 is almost 0.7734
# This means that residuals separated by one year have a correlation of 0.77; by two years it is 0.77^2 = 0.59



# ----------
compareCoefs(M0, M1, M2)



# -->
# Note that standard errors are increased for year and intercept



# ----------
AIC(M0, M1, M2)



# -->
# The AIC indicates that the AR-1 correlation structure is a considerable model improvement compared to the linear regression model.
# In general, you would expect rho to be positive as values at any particular point in time are positively related to preceding time points.
# Ocassionally, you find a negative rho. Plausible explanations are either the model is missing an important explanatory variable or
# the abundances go from high values in one year to low values in the next year.



# ----------
# but almost same residuals
plot(M1)

plot(M2)




