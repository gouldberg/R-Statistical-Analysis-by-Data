setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\dry2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dry2
# ------------------------------------------------------------------------------


u <- read.csv("dry2_u.txt", sep = "", header = F, colClasses = "numeric")

y <- read.csv("dry2_y.txt", sep = "", header = F, colClasses = "numeric")


dry2 <- cbind(u, y)


colnames(dry2) <- c("input", "output")


head(dry2)



# ----------
str(dry2)



# ----------
# detrending
output2 <- resid(lm(output ~ time(output), data = dry2))


# demean
dry2$input2 <- dry2$input - mean(dry2$input)




# ------------------------------------------------------------------------------
# Fit Vector AR model (VAR)
# ------------------------------------------------------------------------------

# vars Fit vector AR models via least squares.
library(vars)


input <- dry2$input2

output <- dry2$output


# later, we test granger causality, so the order is "input" --> "output"
x <- cbind(input, output)



# ----------
# "both" fits constant + trend

VARselect(x, lag.max = 20, type = "both")


# -->
# Note that BIC picks the order p = 5 model while AIC and FPE pick an order p = 6 model
# and Hannan-Quinn selects an order p = 5 model.





# ------------------------------------------------------------------------------
# Fit VAR model by p = 6  with const + trend
# ------------------------------------------------------------------------------

summary(fit6 <- VAR(x, p = 6, type = "both"))




# ------------------------------------------------------------------------------
# Fit VAR model by p = 5  with const + trend
# ------------------------------------------------------------------------------

summary(fit5 <- VAR(x, p = 5, type = "both"))


summary(fit6)


coef(fit5)$output





# ------------------------------------------------------------------------------
# VAR model by  MTS::VARirf()
# ------------------------------------------------------------------------------

varfit_MTS <- MTS::VAR(x, p = 5)


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
