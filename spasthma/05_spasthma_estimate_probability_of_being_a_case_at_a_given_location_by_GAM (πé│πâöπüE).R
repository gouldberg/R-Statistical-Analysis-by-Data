setwd("//media//kswada//MyFiles//R//spasthma")

packages <- c("dplyr", "rgdal", "spatstat", "maptools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  spasthma
# ------------------------------------------------------------------------------
library(rgdal)

spasthma <- readOGR("//media//kswada//MyFiles//data//spasthma", "spasthma")
spbdry <- readOGR("//media//kswada//MyFiles//data//spasthma//spbdry.shp", "spbdry")
spsrc <- readOGR("//media//kswada//MyFiles//data//spasthma//spsrc.shp", "spsrc")
sproads <- readOGR("//media//kswada//MyFiles//data//spasthma//sproads.shp", "sproads")


# ----------
# SpatialPointsDataFrame
str(spasthma)

# SpatialPolygonsDataFrame
str(spbdry)

# SpatialPointsDataFrame
str(spsrc)

# SpatialLinesDataFrame
str(sproads)



# ------------------------------------------------------------------------------
# preparation
# ------------------------------------------------------------------------------
pppasthma <- as(spasthma, "ppp")
pppasthma$window <- as(spbdry, "owin")

marks(pppasthma) <- relevel(pppasthma$marks$Asthma, "control")

pppasthma
str(pppasthma)



# ----------
cases <- unmark(subset(pppasthma, marks(pppasthma) =="case"))
controls <- unmark(subset(pppasthma, marks(pppasthma) =="control"))

ncases <- npoints(cases)
ncontrols <- npoints(controls)



# ------------------------------------------------------------------------------
# Estimate probability of being a case at a given location by GAM (Generalized Additive Models)
#
#  - GAM using the distance of the events to the pollution sources and main roads,
#    and controlling for known and possible risk factors such as gender, age, previous events of hay fever, and having at leasto ne smoker in the house.
#
#  - Kelsall and Diggle (1998) estimate using a kernel weighted regression, but our packages mgcv lacks the same non-parametric estimator used in
#    Kelsall and Diggle (1998), we have preferred the use of a penalised spline instead.
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# preparation
# ------------------------------------------------------------------------------
pppasthma <- as(spasthma, "ppp")
pppasthma$window <- as(spbdry, "owin")

marks(pppasthma) <- relevel(pppasthma$marks$Asthma, "control")

pppasthma
str(pppasthma)



# ----------
cases <- unmark(subset(pppasthma, marks(pppasthma) =="case"))
controls <- unmark(subset(pppasthma, marks(pppasthma) =="control"))

ncases <- npoints(cases)
ncontrols <- npoints(controls)



# ------------------------------------------------------------------------------
# Estimate probability of being a case at a given location by GAM (Generalized Additive Models)
# ------------------------------------------------------------------------------
spasthma$y <- as.integer(!as.integer(spasthma$Asthma) - 1)
ccasthma <- coordinates(spasthma)
spasthma$x1 <- ccasthma[,1]
spasthma$x2 <- ccasthma[,2]
spasthma$dist1 <- sqrt(spasthma$d2source1)
spasthma$dist2 <- sqrt(spasthma$d2source2)
spasthma$dist3 <- sqrt(spasthma$d2source3)
spasthma$droads <- sqrt(spasthma$roaddist2)
spasthma$smoking <- as.factor(as.numeric(spasthma$Nsmokers > 0))
spasthma$Genderf <- as.factor(spasthma$Gender)
spasthma$HayFeverf <- as.factor(spasthma$HayFever)



# ----------
library(mgcv)
gasthma <- gam(y ~ 1 + dist1 + dist2 + dist3 + droads + Genderf + Age + HayFeverf + smoking + s(x1, x2),
               data = spasthma[spasthma$Gender==1 | spasthma$Gender==2, ], family=binomial)


summary(gasthma)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
sumGasth <- summary(gasthma)
cpv <- sumGasth$p.pv
spv <- sumGasth$s.table[4]


# ----------
D2_mat <- as.matrix(spasthma$dist2)
RHO <- ncases / ncontrols
expsource2 <- splancs::tribble(ccflag=spasthma$y, vars=D2_mat, rho=RHO, alphas=1, betas=1)
print(expsource2)



# ----------
# Hay fever
Hay_mat <- as.matrix(spasthma$HayFever)
exphay <- tribble(ccflag=spasthma$y, rho=RHO, covars=Hay_mat, thetas=1)
print(exphay)



# ----------
expsource2hay <- tribble(ccflag=spasthma$y, vars=D2_mat, rho=RHO, alphas=1, betas=1, covars=Hay_mat, thetas=1)
print(expsource2hay)




