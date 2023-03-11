setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ------------------------------------------------------------------------------
# The quasi-poisson model
# ------------------------------------------------------------------------------

mod.qpois <- glm(Species ~ log(Area) + log(Adjacent), data = gala, family = quasipoisson)


summary(mod.qpois)


plot(mod.qpois)

plot_resid(mod=mod.qpois, y = "Species")



# ------------------------------------------------------------------------------
# negative-binomiaol model
# ------------------------------------------------------------------------------

library(MASS)


# ----------
# estimate theta first
( nbin <- glm.nb(Species ~ log(Area) + log(Adjacent), data = gala) )

( theta <- nbin$theta )



# ----------
mod.nbin <- glm(Species ~ log(Area) + log(Adjacent), gala, family = negative.binomial(theta))

summary(mod.nbin)



# ----------
plot(allEffects(mod.nbin), band.colors = "blue", lwd=3, ylab = "Number of articles", main="", ylim=c(0, log(1000)))

plot_resid(mod = mod.nbin, y = "Species")


