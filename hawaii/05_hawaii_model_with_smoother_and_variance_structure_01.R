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

# Other than Moorhen.Kaui

Birds <- c(Hawaii$Stilt.Oahu, Hawaii$Stilt.Maui, Hawaii$Coot.Oahu, Hawaii$Coot.Maui)


Time <- rep(Hawaii$Year, 4)


Rain <- rep(Hawaii$Rainfall, 4)


ID <- factor(rep(c("Stilt.Oahu", "Stilt.Maui", "Coot.Oahu", "Coot.Maui"), each = length(Hawaii$Year)))



# ----------
xyplot(Birds ~ Time | ID, col = 1, type = "o")



# -->
# Some species have considerable more variation, indicating violation of homogeneity




# ------------------------------------------------------------------------------
# Smoother for each time series and different residual spread for each time series
#   - Birds(is) = alpha(i) + beta + Rainfall(is) + f(i)(Year(s)) + error(is)
# ------------------------------------------------------------------------------

library(mgcv)


# by = as.numeric(.): each smoother is only applied to one time series
BM1 <- gamm(Birds ~ Rain + ID +
              s(as.numeric(Time), by = ID), weights = varIdent(form = ~1 | ID))


#BM1 <- gamm(Birds ~ Rain + ID +
#              s(Time, by = as.numeric(ID == "Stilt.Oahu")) + 
#              s(Time, by = as.numeric(ID == "Stilt.Maui")) +
#              s(Time, by = as.numeric(ID == "Cool.Oahu")) +
#              s(Time, by = as.numeric(ID == "Coot.Maui")),
#            weights = varIdent(form = ~1 | ID))



# ----------
summary(BM1$gam)



# -->
# The problme here is that the p-values assume independence and because the data are time series,
# these asumptions may be violated.



# ------------------------------------------------------------------------------
# plot Smoother
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,2))

plot(BM1$gam, scale = FALSE, shade = TRUE)



# -->
# Coot.Oahu have is sinusoida pattern



# ------------------------------------------------------------------------------
# Assess residual pattern
# ------------------------------------------------------------------------------

E <- residuals(BM1$gam)


graphics.off()

par(mfrow = c(1,1))


xyplot(E ~ Time | ID, type = "o")


