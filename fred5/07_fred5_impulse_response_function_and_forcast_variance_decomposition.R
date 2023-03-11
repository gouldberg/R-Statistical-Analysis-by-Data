# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/fred5")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# --> Continued from previous scripts

# ------------------------------------------------------------------------------
# Impulse Responses Function:  How much 1 SD fluctuation of Personal Consumption impacts to Personal Disposable Income ?
# ------------------------------------------------------------------------------
# default is ortho = TRUE
impresp <- vars::irf(varfit)



# ----------
graphics.off()

# Response of Dc and Dy by Dc shock
plot(impresp, plot.type = "m", names = "Dc", main = "", mar=c(gap=0.4, 5.1, gap = 0.4, 2.1), lwd = 1.6)

# Response of Dc and Dy by Dy shock
plot(impresp, plot.type = "m", names = "Dy", main = "", mar=c(gap=0.4, 5.1, gap = 0.4, 2.1), lwd = 1.6)


# -->
# The shock of stock market impacts to only 3-4 days as longest.


# ----------
round(impresp$irf$Dc, digits = 5) * 100


# -->
# the shock of 1 standard deviasion at D(log of Personal Consumption) is 0.43%, 0.18% increase or decrease to D(log of Personal Disposable Income)
# more than VAR predicted values at time zero.
# the impacts are small but continues long

# ----------
impresp$irf$uk



# ------------------------------------------------------------------------------
# Impulse Responses Function by MTS::VARirf()
# ------------------------------------------------------------------------------
# by MTS:  Original and Orthogonal innovations

MTS::VARirf(varfit_MTS_ref$Phi, varfit_MTS_ref$Sigma, orth=F)


MTS::VARirf(varfit_MTS_ref$Phi, varfit_MTS_ref$Sigma)




# ------------------------------------------------------------------------------
# Forecast Variance Decomposition
#    - estimates the contribution of a shock in each variable to the reponse in both variables.
# ------------------------------------------------------------------------------
plot(vars::fevd(varfit))


# -->
# Almost 100% of the variance in Dc is caused by Dc itself, while only abount 80% in the variance of Dy is caused by Dy and rest is caused by Dc.
# Note that the forecst variance explained for Dy by Dc is increased through time period.



# ------------------------------------------------------------------------------
# Forecast ERROR Variance Decomposition
# ------------------------------------------------------------------------------
# by MTS:  Forecast Error Vairance Decomposition
MTS::FEVdec(Phi = varfit_MTS_ref$Phi, Theta = NULL, Sig = varfit_MTS_ref$Sigma, lag = 10)

