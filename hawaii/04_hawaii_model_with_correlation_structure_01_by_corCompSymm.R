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
# Impose correlation structure:  corCompSymm  (Compound Symmetry Correlation Structure):  constant rho among other years
#   - Check the correlation matrix
# ------------------------------------------------------------------------------


library(nlme)



# ----------
# corCompSymm
# for example value = 0.3

cs <- corCompSymm(value = 0.3, form = ~Year)


cs <- Initialize(cs, Hawaii)



# ----------
coef(cs, unconstrained = FALSE)


coef(cs)


getCovariate(cs)


corMatrix(cs)






# ------------------------------------------------------------------------------
# Impose correlation structure:  corCompSymm  (Compound Symmetry Correlation Structure):  constant rho among other years
# ------------------------------------------------------------------------------


M1 <- gls(Birds ~ Rainfall + Year, na.action = na.omit, data = Hawaii, correlation = corCompSymm(form = ~ Year))


summary(M1)



# -->
# Rho is almost 0



# ----------
compareCoefs(M0, M1)


# -->
# Estimated regression parameters and p-values are the same
# as for the ordinary linear regression model



# ----------
AIC(M0, M1)



# -->
# AIC does not improved



# ----------
# almost same residuals
plot(M0)

plot(M1)




