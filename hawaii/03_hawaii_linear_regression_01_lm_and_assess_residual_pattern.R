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



# ------------------------------------------------------------------------------
# Linear Regression by lm for Moorhen.Kauai
# ------------------------------------------------------------------------------


Hawaii$Birds <- sqrt(Hawaii$Moorhen.Kauai)


M00 <- lm(Birds ~ Rainfall + Year, na.action = na.omit, data = Hawaii)


summary(M00)



# -->
# The effect of rainfall is not significant, but there is a significant increase in birds over time.
# The problem is that we cannot trust these p-values as we may be violating the independence assumption.




# ----------
car::residualPlots(M00)




# ----------
car::qqPlot(resid(M00))



# -->
# Violating the independence assumption




# ------------------------------------------------------------------------------
# Assess the residuals autocorrelation
# ------------------------------------------------------------------------------

I1 <- !is.na(Hawaii$Birds)

Efull0 <- vector(length = length(Hawaii$Birds))

Efull0 <- NA

Efull0[I1] <- E



acf2(Efull0, na.action = na.pass, max.lag = 10)


