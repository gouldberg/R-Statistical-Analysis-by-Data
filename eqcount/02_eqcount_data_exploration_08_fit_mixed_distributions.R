setwd("//media//kswada//MyFiles//R//eqcount")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqcount
# ------------------------------------------------------------------------------

data(EQcount, package = "astsa")


str(EQcount)


EQcount




# ------------------------------------------------------------------------------
# data exploration:  fit mixed distributions
# ------------------------------------------------------------------------------

fit_gmx1 <- gamlssMXfits(formula = EQcount ~ 1, K = 1, data = EQcount, family = PO, plot.opt = 0)

fit_gmx2 <- gamlssMXfits(formula = EQcount ~ 1, K = 2, data = EQcount, family = PO, plot.opt = 0)

fit_gmx3 <- gamlssMXfits(formula = EQcount ~ 1, K = 3, data = EQcount, family = PO, plot.opt = 0)

fit_gmx4 <- gamlssMXfits(formula = EQcount ~ 1, K = 4, data = EQcount, family = PO, plot.opt = 0)



# ----------
GAIC(fit_gmx1, fit_gmx2, fit_gmx3, fit_gmx4)

GAIC(fit_gmx1, fit_gmx2, fit_gmx3, fit_gmx4, k = log(length(EQcount)))



# -->
# 2 or 3 mixed distributions



# ----------
fit_gmx3




# ------------------------------------------------------------------------------
# Plot fitted values
# ------------------------------------------------------------------------------

eqc_x <- seq(0, 100, by = 1)


mu <- c(exp(2.537), exp(2.98), exp(3.451))


pi <- c(0.2678837, 0.5999115, 0.1322048)



fyPO <- dMX(y = eqc_x, 
            mu = mu,
            pi = pi,
            family = list("PO", "PO", "PO"))


plot(fyPO ~ eqc_x, type="l")  




# ----------
fyPO1 <- dpois(x = eqc_x, lambda = mu[1])

fyPO2 <- dpois(x = eqc_x, lambda = mu[2])

fyPO3 <- dpois(x = eqc_x, lambda = mu[3])


plot(eqc_x, fyPO1, type="l", lty = 2, col = "black", xlim = c(0, 100), ylim = c(0, 0.15))
lines(eqc_x, fyPO2, type="l", lty = 1, col = "blue")
lines(eqc_x, fyPO3, type="l", lty = 3, col = "red")





# ----------
fn <- getpdfMX(fit_gmx3)


par(mfrow = c(1,1))

MASS::truehist(EQcount, nbins = 30, col = "grey", xlab = "EarthQuake count")

lines(eqc_x, fn(eqc_x), lty = 1, lwd = 2, col = "blue")


