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


Hawaii$Birds <- sqrt(Hawaii$Moorhen.Kauai)





# ------------------------------------------------------------------------------
# Linear Regression by gls for Moorhen.Kauai  (same result)
# ------------------------------------------------------------------------------

library(nlme)


Hawaii$Birds <- sqrt(Hawaii$Moorhen.Kauai)


M0 <- gls(Birds ~ Rainfall + Year, na.action = na.omit, data = Hawaii)


summary(M0)



# -->
# The effect of rainfall is not significant, but there is a significant increase in birds over time.
# The problem is that we cannot trust these p-values as we may be violating the independence assumption.




# ------------------------------------------------------------------------------
# Assess residual pattern
# ------------------------------------------------------------------------------

E <- residuals(M0, type = "normalized")


par(mfrow = c(1,1))
plot(E, type = "o")


# plot(M0)



# ------------------------------------------------------------------------------
# Auto-correlation plot for residuals  (same with lm)
# ------------------------------------------------------------------------------

I1 <- !is.na(Hawaii$Birds)

Efull <- vector(length = length(Hawaii$Birds))

Efull <- NA

Efull[I1] <- E


acf2(Efull, na.action = na.pass, max.lag = 10)



# -->
# These data clearly contain residual correlation
# As a result, we cannot assume that the F-statistic follows an F-distribution and the t-statistic a t-distribution



