setwd("//media//kswada//MyFiles//R//japanesepines")

packages <- c("dplyr", "spatstat", "maptools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  japanesepines
# ------------------------------------------------------------------------------

data("japanesepines", package = "spatstat")

class(japanesepines)

str(japanesepines)



# ----------
# convert to Spatial Points class
spjpines <- as(japanesepines, "SpatialPoints")

str(spjpines)

# Convert to unit square using the elide methods
spjpines1 <- elide(spjpines, scale=TRUE, unitsq=TRUE)



# ----------
data(redwoodfull, package = "spatstat")

class(redwoodfull)

spred <- as(redwoodfull, "SpatialPoints")



# ----------
data(cells, package = "spatstat")

class(cells)

spcells <- as(cells, "SpatialPoints")



# ----------
summary(spjpines1)

summary(spred)

summary(spcells)



# ------------------------------------------------------------------------------
# Fit Point Process model to data:  log intensity model
#
#  - by default of spatstat::ppm(), a Poisson point process is used, but many other point processes can be fitted 
# ------------------------------------------------------------------------------

# log intensity model = alpha + beta1 * x1 + beta2 + x2 + beta3 * x1^2 + beta4 * x2^2 + beta5 * x1 * x2

ppm_r <- ppm(Q = redwoodfull, trend = ~ x + y + I(x^2) + I(y^2) + I(x*y))

ppm_j <- ppm(Q = japanesepines, trend = ~ x + y + I(x^2) + I(y^2) + I(x*y))

ppm_c <- ppm(Q = cells, trend = ~ x + y + I(x^2) + I(y^2) + I(x*y))



# ----------
summary(ppm_r)


summary(ppm_j)


summary(ppm_c)



# ----------
# close to esimated by likelihood method
coef(ppm_r)
optbeta_r$par


coef(ppm_j)
optbeta_j$par


coef(ppm_c)
optbeta_c$par



# ------------------------------------------------------------------------------
# plot fitted trend and estimated SE
# ------------------------------------------------------------------------------

plot(ppm_c)


plot(ppm_r)


plot(ppm_j)

