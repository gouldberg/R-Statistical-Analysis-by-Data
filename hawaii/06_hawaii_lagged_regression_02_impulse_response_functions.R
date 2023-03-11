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
# data exploration:  time series plot
# ------------------------------------------------------------------------------

# Stilt.Maui and Coot.Maui

Birds <- c(Hawaii$Stilt.Maui, Hawaii$Coot.Maui)


Time <- rep(Hawaii$Year, 2)


Rain <- rep(Hawaii$Rainfall, 2)


ID <- factor(rep(c("Stilt.Maui", "Coot.Maui"), each = length(Hawaii$Year)))



# ----------
xyplot(Birds ~ Time | ID, col = 1, type = "o")



# ----------
Birds_sm <- Hawaii$Stilt.Maui

Birds_cm <- Hawaii$Coot.Maui


Birds_so <- Hawaii$Stilt.Oahu

Birds_co <- Hawaii$Coot.Oahu



# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function
# ------------------------------------------------------------------------------


# estimated regression or impulse response function for Coot.Maui
# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


mod_stoc <- astsa::LagReg(input = Birds_sm, output = Birds_cm, L = 15, M = 10)





# ----------
# Examine inverse relation, namely, a regression model with the Birds_cm series as input --> bad....

mod_ctos <- astsa::LagReg(input = Birds_cm, output = Birds_sm, L = 3, M = 10, inverse = TRUE)




