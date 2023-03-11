setwd("//media//kswada//MyFiles//R//rent")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rent
# ------------------------------------------------------------------------------
data("rent", package = "gamlss.data")


str(rent)

car::some(rent)



# ------------------------------------------------------------------------------
# Model varying coefficients interaction by pvc()
# ------------------------------------------------------------------------------

# main smoothing effects for Fl and A
m0 <- gamlss(R ~ pb(Fl) + pb(A), data = rent, family = GA)


# linear interaction between A and Fl
m1 <- gamlss(R ~ pb(Fl) + pb(A) + A:Fl, data = rent, family = GA)


# varying coefficients interaction b(A) Fl
m2 <- gamlss(R ~ pb(Fl) + pb(A) + pvc(A, by = Fl), data = rent, family = GA)


# varying coefficients interaction b(A) A
m3 <- gamlss(R ~ pb(Fl) + pb(A) + pvc(Fl, by = A), data = rent, family = GA)


# linear interaction plus varying coefficients interaction b(A) Fl
m4 <- gamlss(R ~ pb(Fl) + pb(A) + A:Fl + pvc(A, by = A), data = rent, family = GA)



# ----------
GAIC(m0, m1, m2, m3, m4, r6, r7)



# ------------------------------------------------------------------------------
# Plot the relationship of the estimated smooth coefficient beta agains x, as shown in the varying coef plot
# ------------------------------------------------------------------------------

term.plot(m2, pages = 1, ask = FALSE)



# ------------------------------------------------------------------------------
# Plot the relationship in 2-D by contour plot
# ------------------------------------------------------------------------------

newrent <- data.frame(expand.grid(Fl = seq(30, 120, 5), A = seq(1890, 1900, 5)))

newrent$pred <- predict(m2, newdata = newrent, type = "response", data = rent)



# ----------
Fln <- seq(30, 120, 5)
An <- seq(1890, 1990, 5)

op <- par(mfrow=c(1,1))
contour(Fln, An, matrix(newrent$pred, nrow = length(Fln)), nlevels = 30, ylab = "year of construction", xlab = "floor space")
par(op)






