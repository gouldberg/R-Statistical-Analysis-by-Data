# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R/msci_day")


packages <- c("dplyr", "AER", "stargazer", "broom", "knitr", "tseries", "vars", "MTS", "forecast")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# --> Continued from previous scripts

# ------------------------------------------------------------------------------
# Impulse Responses Function:  How much 1 SD fluctuation impacts to other market ?
# ------------------------------------------------------------------------------
# default is ortho = TRUE
impresp <- vars::irf(varfit)



# ----------
graphics.off()

# Response of JP, FR and CA by JP shock
plot(impresp, plot.type = "m", names = "jp", main = "", mar=c(gap=0.4, 5.1, gap = 0.4, 2.1), lwd = 1.6)

# Response of JP, FR and CA by FR shock
plot(impresp, plot.type = "m", names = "fr", main = "", mar=c(gap=0.4, 5.1, gap = 0.4, 2.1), lwd = 1.6)

# Response of JP, FR and CA by CA shock
plot(impresp, plot.type = "m", names = "ca", main = "", mar=c(gap=0.4, 5.1, gap = 0.4, 2.1), lwd = 1.6)


# -->
# The shock of stock market impacts to only 3 days as longest.


# ----------
round(impresp$irf$jp, digits = 5)*100


# -->
# the shock of 1 standard deviasion at Japan market is 1.2% to the JP market, 0.28% to FR market, 0.18% to CA market at the day
# meaning that even if the JP stock market fluctuates around 1.2% compared to VAR predicted values, the impact to FR and CA market is very limited.


# ----------
round(impresp$irf$fr, digits = 5)*100


# -->
# After the shock of 1 standard deviasion at FR (1.095% to the FR market compared to VAR predicted values),
# JP market will be increased (decreased) 0.47%, and CA market will be increased (decreased) 0.54% (as expected) more than VAR predicted values



# ----------
round(impresp$irf$ca, digits = 5)*100


# -->
# After the shock of 1 standard deviasion at CA (0.867% to the CA market compared to VAR predicted values),
# JP market will be increased (decreased) 0.22%, and UK market will be increased (decreased) 0.19% (as expected) more than VAR predicted values



# ------------------------------------------------------------------------------
# Impulse Responses Function by MTS::VARirf()
# ------------------------------------------------------------------------------
# by MTS:  Original and Orthogonal innovations:  by column JP causes, FR causes, CA causes
# AND Variance Decomposition:  by column JP ratio in JP/FR/CA, FR ratio in JP/FR/CA and CA ratio in JP/FR/CA

MTS::VARirf(varfit_MTS_ref$Phi, varfit_MTS_ref$Sigma, orth=F)


MTS::VARirf(varfit_MTS_ref$Phi, varfit_MTS_ref$Sigma)




# ------------------------------------------------------------------------------
# Forecast Variance Decomposition
#    - estimates the contribution of a shock in each variable to the reponse in both variables.
# ------------------------------------------------------------------------------
plot(vars::fevd(varfit))


# -->
# NOTE that:  Variance Decomposition is almost horizontal, indicating that the forecast variance decomposition is not much dependent on the prediction length of days
# --> 
# the prediction of stock value by current shock is difficult.
# the prediction after 2 days is almost same with expected values without condition. 



# ------------------------------------------------------------------------------
# Forecast ERROR Variance Decomposition
# ------------------------------------------------------------------------------
# by MTS:  Forecast Error Vairance Decomposition
MTS::FEVdec(Phi = varfit_MTS_ref$Phi, Theta = NULL, Sig = varfit_MTS_ref$Sigma, lag = 10)

