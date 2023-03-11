setwd("//media//kswada//MyFiles//R//swer")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Extreme rainfall in Switzerland
# ------------------------------------------------------------------------------

data(swer, package = "gamair")

str(swer)


head(swer)

car::some(swer)




# ------------------------------------------------------------------------------
# generalized additive model by each variable:  nao
# ------------------------------------------------------------------------------

library(mgcv)


gam_nao <- gam(list(exra ~ s(nao) + climate.region,
                    ~ s(nao),
                    ~ s(nao)), 
                    family = gevlss, data = swer)


summary(gam_nao)


graphics.off()
par(mfrow = c(2,2))
plot(gam_nao, shade = TRUE)




# ------------------------------------------------------------------------------
# generalized additive model by each variable:  elevation
# ------------------------------------------------------------------------------


gam_elev <- gam(list(exra ~ s(elevation) + climate.region,
                    ~ s(elevation),
                    ~ s(elevation)), 
               family = gevlss, data = swer)


summary(gam_elev)


graphics.off()
par(mfrow = c(2,2))
plot(gam_elev, shade = TRUE)





# ------------------------------------------------------------------------------
# generalized additive model by each variable:  year
# ------------------------------------------------------------------------------


gam_yr <- gam(list(exra ~ s(year) + climate.region,
                     ~ s(year),
                     ~ s(year)), 
                family = gevlss, data = swer)



summary(gam_yr)


graphics.off()
par(mfrow = c(2,2))
plot(gam_yr, shade = TRUE)




# ------------------------------------------------------------------------------
# generalized additive model by each variable:  N, E
# ------------------------------------------------------------------------------


gam_NE <- gam(list(exra ~ s(N, E) + climate.region,
                   ~ s(N, E),
                   ~ s(N, E)), 
              family = gevlss, data = swer)



summary(gam_NE)


graphics.off()
par(mfrow = c(2,2))
plot(gam_NE, scheme = 1)


graphics.off()
par(mfrow = c(2,2))
plot(gam_NE, scheme = 2)


graphics.off()
par(mfrow = c(2,2))
plot(gam_NE, scheme = 4)



# ------------------------------------------------------------------------------
# generalized additive model by each variable:  N, E and year
# ------------------------------------------------------------------------------


gam_NEY <- gam(list(exra ~ te(N, E, year, d = c(2,1), k = c(20, 5)) + climate.region,
                   ~ s(N, E),
                   ~ s(N, E)), 
              family = gevlss, data = swer)



summary(gam_NEY)


graphics.off()
par(mfrow = c(2,2))
plot(gam_NEY, scheme = 1)


graphics.off()
par(mfrow = c(2,2))
plot(gam_NEY, scheme = 2)


graphics.off()
par(mfrow = c(2,2))
plot(gam_NEY, scheme = 4)

