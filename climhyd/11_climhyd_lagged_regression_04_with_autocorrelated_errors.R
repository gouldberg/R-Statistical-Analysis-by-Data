setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)


inf <- log(climhyd$Inflow)

prec <- sqrt(climhyd$Precip)




# ------------------------------------------------------------------------------
# Asssess residuals
# ------------------------------------------------------------------------------

acf2(resid(u))





# ------------------------------------------------------------------------------
# ARIMAX model
# ------------------------------------------------------------------------------

# with autocorrelated errors: AR(2)

mod0 <- sarima(clim[,1], 1, 0, 1, xreg = clim[,2:6])


mod0



# ----------
# excluding MA terms

mod1 <- sarima(clim[,1], 1, 0, 0, xreg = clim[,2:6])


mod1




# ----------
mod0$AICc

mod1$AICc





