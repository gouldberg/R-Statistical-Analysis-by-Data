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
# F statistics and Regression Coefficients
#   - Regression and Partial R statistics
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(3,1), mar = c(3,3,2,1) + 0.5, mgp = c(1.5, 0.4, 0), cex.lab = 1.2)


# Partial F (called eF; avoid use of F alone)
# Multiple coherency
# stoch.reg:  frequency domain stochasitc regression  --> plot.which = "F.stat"


out.15 <- stoch.reg(Powerplant, cols.full = c(1,5), cols.red = 5, alpha, L, M, plot.which = "F.stat")




# ------------------------------------------------------------------------------
# F statistics and Regression Coefficients
#   - Partial F statistics (ef)
#     Test if temperature is predictive of infrlow when precipitation is in the model ? 
# ------------------------------------------------------------------------------

eF <- out.15$eF

number.df <- 2 * nq

denom.df <- Yspec$df - 2 * nq

pvals <- pf(eF, number.df, denom.df, lower.tail = FALSE)


pID <- FDR(pvals, 0.001)

abline(h = c(eF[pID]), lty = 2)

title(main = "Partial F Statistic")



# -->
# dashed line:  threshold values corresponding to false discovery rate = 0.001 level
# solid line: represents the corresponding quantile of the null F distribution



# ------------------------------------------------------------------------------
# F statistics and Regression Coefficients
#    - Regression Coefficients for Temp and Precip
# ------------------------------------------------------------------------------

# Multiple impulse response functions for the regression relations of temperature and precipitation

S <- seq(from = -M / 2 + 1, to = M / 2 - 1, length = M - 1)

plot(S, coh.15$Betahat[,1], type = "h", xlab = "", ylab = names(climhyd[1]), ylim = c(-0.025, 0.055), lwd = 2)
abline(h = 0)
title(main = "Impulse Response Functions")


plot(S, coh.15$Betahat[,2], type = "h", xlab = "Index", ylab = names(climhyd[5]), ylim = c(-0.015, 0.055), lwd = 2)
abline(h = 0)



# -->
# The time index runs over both positive and negative values and are centered at time t = 0
# The relation with temperature seems to be instantaneous and positive and an exponentially decaying relation to precipitation
# exists that has been noticed in precious analysis

# The plots suggest a transfer function model, without the temperatreu component
# I(t) = alpha0 + delta0 / (1 - omega1 * B) * P(t) + alpha2 * T2 + eta2



