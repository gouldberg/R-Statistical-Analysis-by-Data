setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)



# ------------------------------------------------------------------------------
# data transformation
# ------------------------------------------------------------------------------

apply(climhyd, MARGIN = 2, FUN = forecast::ndiffs)



# -->
# DewPt and WndSped needs to 1st difference to be stationary




# ----------
# Check Box-Cox Transformation
apply(climhyd, MARGIN = 2, FUN = caret::BoxCoxTrans)



car::symbox(climhyd$Inflow)

car::symbox(climhyd$Precip)




# ----------
# Apply log and sqrt to Inflow and Precip respectively,
# although "No transformation is applied" to Precip based on the check of Box-Cox Transformation

inf <- log(climhyd$Inflow)

prec <- sqrt(climhyd$Precip)


par(mfrow = c(2,2))
plot(climhyd$Precip, type = "l", main = "original Precip")
plot(climhyd$Inflow, type = "l", main = "original Inflow")
plot(prec, type = "l", main = "transformed sqrt(Precip)")
plot(inf, type = "l", main = "transformed log(Inflow)")


apply(cbind(inf, prec), MARGIN = 2, FUN = forecast::ndiffs)



