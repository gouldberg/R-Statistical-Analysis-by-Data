setwd("//media//kswada//MyFiles//R//db")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  db
# ------------------------------------------------------------------------------
data("db", package = "gamlss.data")


str(db)

car::some(db)




# ------------------------------------------------------------------------------
# Fit P-spline smoothers for four different methods of estimating the smoothing parameter
# ------------------------------------------------------------------------------


p1 <- gamlss(head ~ pb(age, method = "ML"), data = db, trace = TRUE)

p2 <- gamlss(head ~ pb(age, method = "GCV"), data = db, trace = TRUE)

p3 <- gamlss(head ~ pb(age, method = "GAIC", k = 2), data = db, trace = TRUE)


# In general for large data and for smoother fitted curves, use a local SBC as a selection procedure for the smoothing parameter
p4 <- gamlss(head ~ pb(age, method = "GAIC", k = log(length(db$head))), data = db, trace = TRUE)



# ----------
graphics.off()

par(mfrow = c(2,2))
plot(head ~ age, data = db, cex = 0.2, col = "gray", main = "ML")
lines(fitted(p1) ~ db$age, lwd = 2)

plot(head ~ age, data = db, cex = 0.2, col = "gray", main = "GCV")
lines(fitted(p2) ~ db$age, lwd = 2)

plot(head ~ age, data = db, cex = 0.2, col = "gray", main = "AIC")
lines(fitted(p3) ~ db$age, lwd = 2)

plot(head ~ age, data = db, cex = 0.2, col = "gray", main = "SBC")
lines(fitted(p4) ~ db$age, lwd = 2)



# ----------
getSmo(p4)

summary(p4)
