setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\film_process")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Film Extrusion Process
# ------------------------------------------------------------------------------


film <- read.table("FilmProcess.txt", sep = "", header = F, colClasses = "numeric")


head(film)



# ----------
hc <- film$V1

vp <- film$V2

prs <- film$V3

wid <- film$V4




# ----------------------------------------------------------------------------
# Fit VARMA model by Spliid algorithm by marima()
#   - Spliid algorithm uses the multivariate regression equations.
#     Experience suggests the estimators can be reasonably close to the maximum likelihood estimators.
#     The algorithm can be considered as a quick and easy way to fit an initial VARMA model as a starting point
#     to using maximum likelihood estimation, which is best done via state-space models
# ------------------------------------------------------------------------------


library(marima)


model <- define.model(kvar = 3, ar = c(1,2), ma = c(1))

arp <- model$ar.pattern

map <- model$ma.pattern




# ----------
# detrend prs
prs.d <- resid(detr <- lm(prs ~ time(prs), na.action = NULL))


# strip ts attributes
xdata <- matrix(cbind(prs.d, hc, vp), ncol = 3)



# ----------
fit <- marima(xdata, ar.pattern = arp, ma.pattern = map, means = c(0, 1, 1), penalty = 1)


fit




# ------------------------------------------------------------------------------
# residual analysis
# ------------------------------------------------------------------------------

innov <- t(resid(fit))

plot.ts(innov)



# ----------
acf(innov, na.action = na.pass)




# ------------------------------------------------------------------------------
# fitted values for prs
# ------------------------------------------------------------------------------

pred_varma <- ts(t(fitted(fit))[,1], start = start(prs), freq = frequency(prs)) + detr$coef[1] + detr$coef[2] * time(prs)


par(mfrow = c(1,1))

plot(pred_varma, ylab = "Extrusion Process: Pressure", lwd = 2, col = "blue")
lines(time(prs), c(rep(NA,10), pred_lagr), lwd = 2, col = "red")

points(prs)



# -->
# almost same with lagged regression




# ----------
# print estimates and corresponding t^2-statistic

short.form(fit$ar.estimates, leading = FALSE)

short.form(fit$ar.fvalues, leading = FALSE)

short.form(fit$ma.estimates, leading = FALSE)

short.form(fit$ma.fvalues, leading = FALSE)




# ----------
# estimate of noise cov matrix

fit$resid.cov

