setwd("//media//kswada//MyFiles//R//lynx")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lynx
# ------------------------------------------------------------------------------

data(lynx, package = "datasets")


str(lynx)


lynx



# ----------
lynxl <- log10(lynx)

dlynxl <- diff(log10(lynx))




# ------------------------------------------------------------------------------
# correlation analysis
# ------------------------------------------------------------------------------


astsa::acf2(lynxl)


astsa::acf2(dlynxl)



# -->
# strong auto-correlation



# ----------
astsa::sarima(lynxl, p = 2, d = 1, q = 1, P = 0, D = 0, Q = 4, S = 5, no.constant = TRUE)


astsa::sarima(lynxl, p = 2, d = 1, q = 1, P = 0, D = 0, Q = 2, S = 5, no.constant = TRUE)


astsa::sarima(lynxl, p = 2, d = 0, q = 0)





# ----------
acf2(lynx)

astsa::sarima(lynx, p = 2, d = 0, q = 1, P = 0, D = 0, Q = 4, S = 5)

