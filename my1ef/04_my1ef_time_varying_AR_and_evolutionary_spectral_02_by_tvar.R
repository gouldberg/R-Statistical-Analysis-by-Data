setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\my1ef")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MYE1F
# ------------------------------------------------------------------------------


dat <- read.table("MYE1F.txt", sep = "", header = T, colClasses = "numeric")


head(dat)


dat <- dat$x




# ------------------------------------------------------------------------------
# Time-varying AR by tvReg
# ------------------------------------------------------------------------------

library(tvReg)


# we use normalized data
# est = "ll": local linear,  "lc": default


# AR order = 8
model.tvAR.8plc <- tvAR(dat, p = 8, type = "none", est = "lc")



# ----------
cf <- data.frame(model.tvAR.8plc$coefficients)

head(cf)


graphics.off()

par(mfrow = c(3,3))

# at 315 primary waves arrived and at 513 shear waves arrived
plot(dat, type = "l")
abline(v = c(630, 1026), lty = 1, col = "blue")

plot(cf$y.l1, type = "l")
abline(v = c(630, 1026), lty = 1, col = "blue")

plot(cf$y.l2, type = "l")
abline(v = c(630, 1026), lty = 1, col = "blue")

plot(cf$y.l3, type = "l")
abline(v = c(630, 1026), lty = 1, col = "blue")

plot(cf$y.l4, type = "l")
abline(v = c(630, 1026), lty = 1, col = "blue")

plot(cf$y.l5, type = "l")
abline(v = c(630, 1026), lty = 1, col = "blue")

plot(cf$y.l6, type = "l")
abline(v = c(630, 1026), lty = 1, col = "blue")

plot(cf$y.l7, type = "l")
abline(v = c(630, 1026), lty = 1, col = "blue")

plot(cf$y.l8, type = "l")
abline(v = c(630, 1026), lty = 1, col = "blue")



# ----------
# 80% confidence interval using normal wild bootstrap for object of the
# class attribute tvar with 200 bootstrap resamples

model.tvAR.8plc.b <- confint(model.tvAR.8plc, tboot = "wild2", level = 0.8, runs = 50)


cf <- data.frame(model.tvAR.8plc.b$coefficients)
cf_up <- data.frame(model.tvAR.8plc.b$Upper)
cf_lo <- data.frame(model.tvAR.8plc.b$Lower)



# ----------
# time varying coefficients for linear local AR(2) and its confidence interval (80%)
graphics.off()

par(mfrow = c(3,3))

# at 315 primary waves arrived and at 513 shear waves arrived
plot(dat, type = "l")
abline(v = c(630, 1026), col = "blue", lty = 2)

plot(cf$y.l1, type = "l")
lines(time(cf_up$y.l1), cf_up$y.l1, lty = 2, col = "blue")
lines(time(cf_lo$y.l1), cf_lo$y.l1, lty = 2, col = "blue")
abline(v = c(630, 1026), col = "blue", lty = 2)

plot(cf$y.l2, type = "l")
lines(time(cf_up$y.l2), cf_up$y.l2, lty = 2, col = "blue")
lines(time(cf_lo$y.l2), cf_lo$y.l2, lty = 2, col = "blue")
abline(v = c(630, 1026), col = "blue", lty = 2)

plot(cf$y.l3, type = "l")
lines(time(cf_up$y.l3), cf_up$y.l3, lty = 2, col = "blue")
lines(time(cf_lo$y.l3), cf_lo$y.l3, lty = 2, col = "blue")
abline(v = c(630, 1026), col = "blue", lty = 2)

plot(cf$y.l4, type = "l")
lines(time(cf_up$y.l4), cf_up$y.l4, lty = 2, col = "blue")
lines(time(cf_lo$y.l4), cf_lo$y.l4, lty = 2, col = "blue")
abline(v = c(630, 1026), col = "blue", lty = 2)

plot(cf$y.l5, type = "l")
lines(time(cf_up$y.l5), cf_up$y.l5, lty = 2, col = "blue")
lines(time(cf_lo$y.l5), cf_lo$y.l5, lty = 2, col = "blue")
abline(v = c(630, 1026), col = "blue", lty = 2)

plot(cf$y.l6, type = "l")
lines(time(cf_up$y.l6), cf_up$y.l6, lty = 2, col = "blue")
lines(time(cf_lo$y.l6), cf_lo$y.l6, lty = 2, col = "blue")
abline(v = c(630, 1026), col = "blue", lty = 2)

plot(cf$y.l7, type = "l")
lines(time(cf_up$y.l7), cf_up$y.l7, lty = 2, col = "blue")
lines(time(cf_lo$y.l7), cf_lo$y.l7, lty = 2, col = "blue")
abline(v = c(630, 1026), col = "blue", lty = 2)

plot(cf$y.l8, type = "l")
lines(time(cf_up$y.l8), cf_up$y.l8, lty = 2, col = "blue")
lines(time(cf_lo$y.l8), cf_lo$y.l8, lty = 2, col = "blue")
abline(v = c(630, 1026), col = "blue", lty = 2)





# ------------------------------------------------------------------------------
# Evolutionary spetra
# ------------------------------------------------------------------------------


cf <- data.frame(model.tvAR.8plc$coefficients)

head(cf)



graphics.off()

par(mfrow = c(6,6), mar = c(2,2,2,2))

( obj <- seq(1,1201,50) )

for(i in obj){
  arma.spec(ar = unname(unlist(cf[i,])), log = "yes", main = paste0("time: ", i))
  print(paste0("processing: ", i))
}



( obj <- seq(1251,2600,50) )

for(i in obj){
  arma.spec(ar = unname(unlist(cf[i,])), log = "yes", main = paste0("time: ", i))
  print(paste0("processing: ", i))
}



# -->
# when primary waves arrives at 315
# when Shear waves arrives at 513



