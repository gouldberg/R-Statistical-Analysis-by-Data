
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Furnace
# ------------------------------------------------------------------------------


gasf <- read.table("GasFurnace.txt", sep = "", header = F, colClasses = "numeric")


colnames(gasf) <- c("input", "output")


head(gasf)




# ----------
# VAR model

library(vars)


input <- gasf$input

output <- gasf$output


x <- cbind(input, output)

fit3 <- VAR(x, p = 3, type = "both")

fit6 <- VAR(x, p = 6, type = "both")




# ------------------------------------------------------------------------------
# Impulse Responses Function:  How much 1 SD fluctuation impacts to output ?
# ------------------------------------------------------------------------------

# default is ortho = TRUE

impresp <- vars::irf(fit6, n.ahead = 25)



# ----------
graphics.off()


plot(impresp, plot.type = "m")


round(impresp$irf$input, 3)

round(impresp$irf$output, 3)




# ------------------------------------------------------------------------------
# Impulse Responses Function by MTS::VARirf()
# ------------------------------------------------------------------------------


MTS::VARirf(varfit_MTS_ref$Phi, varfit_MTS_ref$Sigma, orth=F, lag = 25)


MTS::VARirf(varfit_MTS_ref$Phi, varfit_MTS_ref$Sigma, lag = 25)




# ------------------------------------------------------------------------------
# Forecast Variance Decomposition
#    - estimates the contribution of a shock in each variable to the reponse in both variables.
# ------------------------------------------------------------------------------

fvd <- vars::fevd(fit6, n.ahead = 25)

fvd$output


plot(fvd)



# -->
# output at lag 6 is explained only 57.8% by input




# ------------------------------------------------------------------------------
# Forecast ERROR Variance Decomposition
# ------------------------------------------------------------------------------

# by MTS:  Forecast Error Vairance Decomposition

MTS::FEVdec(Phi = varfit_MTS_ref$Phi, Theta = NULL, Sig = varfit_MTS_ref$Sigma, lag = 25)

