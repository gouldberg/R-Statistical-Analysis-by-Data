
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Furnace
#   - Series of input and output measurements on a fas furnace that have been previously modeled by spectral analysis
#     in Jenkins and Watts (1968), and by lagged correlation methods in Box and Jenkins (1970).
# ------------------------------------------------------------------------------


gasf <- read.table("GasFurnace.txt", sep = "", header = F, colClasses = "numeric")


colnames(gasf) <- c("input", "output")


head(gasf)




# ------------------------------------------------------------------------------
# Fit Vector AR model (VAR)
# ------------------------------------------------------------------------------

# vars Fit vector AR models via least squares.
library(vars)


input <- gasf$input

output <- gasf$output


# later, we test granger causality, so the order is "input" --> "output"
x <- cbind(input, output)



# ----------
# "both" fits constant + trend

VARselect(x, lag.max = 10, type = "both")


# -->
# Note that BIC picks the order p = 3 model while AIC and FPE pick an order p = 6 model
# and Hannan-Quinn selects an order p = 4 model.





# ------------------------------------------------------------------------------
# Fit VAR model by p = 3  with const + trend
# ------------------------------------------------------------------------------

summary(fit3 <- VAR(x, p = 3, type = "both"))




# ------------------------------------------------------------------------------
# Fit VAR model by p = 6  with const + trend
# ------------------------------------------------------------------------------

summary(fit6 <- VAR(x, p = 6, type = "both"))


summary(fit6)


coef(fit6)$output





# ------------------------------------------------------------------------------
# VAR model by  MTS::VARirf()
# ------------------------------------------------------------------------------

varfit_MTS <- MTS::VAR(x, p = 6)


varfit_MTS



# ----------
# Model Simplification (Refinement)
# The command uses a threshold to select target parameters for removal and computes information criteria
# of the simplified model for validation.
# The default threshold is 1.00
# The command also allows the user to specify zero parameters with a subcommand fixed.


varfit_MTS_ref <- refVAR(varfit_MTS, thres = 1.96)


varfit_MTS_ref$coef



# ----------
# simplified model is better

varfit_MTS$aic;  varfit_MTS$bic;

varfit_MTS_ref$aic;  varfit_MTS_ref$bic;



MTS::VARirf(varfit_MTS_ref$Phi, varfit_MTS_ref$Sigma, orth=F)
