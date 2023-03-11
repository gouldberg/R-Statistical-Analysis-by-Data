setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



# ------------------------------------------------------------------------------
# Assess the effect of changing smoother
# ------------------------------------------------------------------------------
# In case over-smoothing is suspected, check the effect of changing smoother
# we also fit a cubic regression model rather than the thin plate regression spline.
M3 <- gam(d15N ~ Lat + s(ML, bs = "cr"), data = Squid)

summary(M3)


# -->
# The resulting smoother has 2.57 degrees of freedom and is indistinguishable from previous model.



# ----------
# P-spline
M3B <- gam(d15N ~ Lat + s(ML, bs = "ps"), data = Squid)

summary(M3B)



# ----------
# cubic regression spline with shrinkage
M3C <- gam(d15N ~ Lat + s(ML, bs = "cs"), data = Squid)

summary(M3C)



# ----------
# Almost same output
par(mfrow=c(2,2))
plot(M2, resid = TRUE)
plot(M3, resid = TRUE)
plot(M3B, resid = TRUE)
plot(M3C, resid = TRUE)

