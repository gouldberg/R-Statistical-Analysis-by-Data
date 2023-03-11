setwd("//media//kswada//MyFiles//R//abdom")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  abdom
# ------------------------------------------------------------------------------
data("abdom", package = "gamlss.data")


str(abdom)

car::some(abdom)



# ------------------------------------------------------------------------------
# Fit P-spline smoother by re()
# ------------------------------------------------------------------------------

a1 <- gamlss(y ~ pb(x), data = abdom, trace = FALSE)




# ----------
( Z <- getZmatrix(abdom$x) )

Ida <- factor(rep(1, 610))


# The random effect is declared at the observation level by creating and using the factor Ida
# Sigma_b is estimated by declaring the identity matrix pdIdent
a2 <- gamlss(y ~ 1 + re(fixed = ~x, random = list(Ida = pdIdent(~Z-1)), method = "REML"), data = abdom, trace = FALSE)




# ----------
getSmo(a1)

getSmo(a2)



# -->
# getSmo() shows that the estimates for sigma_b in models a1 and a2 are identical.



# ----------
graphics.off()
par(mfrow = c(1,1))

plot(y ~ x, data = abdom, cex = 0.2)

lines(a1$mu.fv[order(abdom$x)] ~ sort(abdom$x), col = "red", lty = 2)
lines(a2$mu.fv[order(abdom$x)] ~ sort(abdom$x), col = "blue", lty = 3)

legend("topleft", legend = c("model a1 (using pb())", "model a2 (using re())"), lty = 2:3, col = c("red", "blue"), lwd = 2, cex = 1.5)



