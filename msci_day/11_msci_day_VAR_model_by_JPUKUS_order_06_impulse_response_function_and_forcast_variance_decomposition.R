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

# Response of JP, UK and US by JP shock
plot(impresp, plot.type = "m", names = "jp", main = "", mar=c(gap=0.4, 5.1, gap = 0.4, 2.1), lwd = 1.6)

# Response of JP, UK and US by UK shock
plot(impresp, plot.type = "m", names = "uk", main = "", mar=c(gap=0.4, 5.1, gap = 0.4, 2.1), lwd = 1.6)

# Response of JP, UK and US by US shock
plot(impresp, plot.type = "m", names = "us", main = "", mar=c(gap=0.4, 5.1, gap = 0.4, 2.1), lwd = 1.6)


# -->
# The shock of stock market impacts to only 3-4 days as longest.


# ----------
round(impresp$irf$jp * 100, 3)


# -->
# the shock of 1 standard deviasion at Japan market is 1.1% to the JP market, 1.6% to UK market, 0.08% to US market at the day
# meaning that even if the JP stock market fluctuates around 1.2% compared to VAR predicted values, the impact to UK and US market is very limited.


# ----------
round(impresp$irf$uk * 100, 3)


# -->
# After the shock of 1 standard deviasion at UK (0.95% to the UK market compared to VAR predicted values),
# JP market will be increased (decreased) 0.41%, and US market will be increased (decreased) 0.40% (as expected) more than VAR predicted values



# ----------
round(impresp$irf$us * 100, 3)


# -->
# After the shock of 1 standard deviasion at UK (0.75% to the US market compared to VAR predicted values),
# JP market will be increased (decreased) 0.39%, and UK market will be increased (decreased) 0.38% (as expected) more than VAR predicted values



# ------------------------------------------------------------------------------
# Impulse Responses Function by MTS::VARirf()
# ------------------------------------------------------------------------------
# by MTS:  Original and Orthogonal innovations:  by column JP causes, UK causes, US causes
# AND Variance Decomposition:  by column JP ratio in JP/UK/US, UK ratio in JP/UK/US and US ratio in JP/UK/US

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

# Unpredictable JP variance is explaned by 10.2% by UK and US by 9.9%.
# meaning that almost 80% of variance is explained by JP factor. (not by other countries market factor)

# For US stock market, UK explains 22% of US variance, but JP explains almost zero.



# ------------------------------------------------------------------------------
# Forecast ERROR Variance Decomposition
# ------------------------------------------------------------------------------
# by MTS:  Forecast Error Vairance Decomposition
MTS::FEVdec(Phi = varfit_MTS_ref$Phi, Theta = NULL, Sig = varfit_MTS_ref$Sigma, lag = 10)

