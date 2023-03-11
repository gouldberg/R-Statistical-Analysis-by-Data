setwd("//media//kswada//MyFiles//R//mack")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  mack
# ------------------------------------------------------------------------------

data(mack, package = "gamair")

data(mackp, package = "gamair")

data(coast, package = "gamair")


str(mack)

str(mackp)

str(coast)



# ----------
# mackp contains prediction grid data for 1992 mackerel egg model.
# A data from with 5 columns. Each row corresponds to one spatial location within the survey area.

# coast:  European coastline from -11 to 0 East and from 43 to 59 North



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


summary(mack)


# -->
# there are many missing values for salinity (304 !!!)




# ----------
table(mack$time)


table(mack$country)


table(mack$vessel)



# ----------
psych::describe(mack)



# ----------
plot(coast$lon, coast$lat, type="l", col = "blue")
# or draw it clipped to  whatever the current plot is....
# lines(coast$lon, coast$lat, col="blue")

par(new = T)
plot(lat ~ lon, data = mackp, cex = 0.3)

