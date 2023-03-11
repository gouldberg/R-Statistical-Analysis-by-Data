setwd("//media//kswada//MyFiles//R//dbbmi")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dbbmi
# ------------------------------------------------------------------------------
data("dbbmi", package = "gamlss.data")


str(dbbmi)

car::some(dbbmi)



# ------------------------------------------------------------------------------
# Fit P-spline smoothers for four different methods of estimating the smoothing parameter
# ------------------------------------------------------------------------------


p1 <- gamlss(bmi ~ pb(age, method = "ML"), data = dbbmi, trace = TRUE)

p2 <- gamlss(bmi ~ pb(age, method = "GCV"), data = dbbmi, trace = TRUE)

p3 <- gamlss(bmi ~ pb(age, method = "GAIC", k = 2), data = dbbmi, trace = TRUE)


# In general for large data and for smoother fitted curves, use a local SBC as a selection procedure for the smoothing parameter
p4 <- gamlss(bmi ~ pb(age, method = "GAIC", k = log(length(dbbmi$bmi))), data = dbbmi, trace = TRUE)



# ----------
par(mfrow = c(2,2))
plot(bmi ~ age, data = dbbmi, cex = 0.2, col = "gray", main = "ML")
lines(fitted(p1) ~ dbbmi$age, lwd = 2)

plot(bmi ~ age, data = dbbmi, cex = 0.2, col = "gray", main = "GCV")
lines(fitted(p2) ~ dbbmi$age, lwd = 2)

plot(bmi ~ age, data = dbbmi, cex = 0.2, col = "gray", main = "AIC")
lines(fitted(p3) ~ dbbmi$age, lwd = 2)

plot(bmi ~ age, data = dbbmi, cex = 0.2, col = "gray", main = "SBC")
lines(fitted(p4) ~ dbbmi$age, lwd = 2)



# ----------
getSmo(p4)

summary(p4)
