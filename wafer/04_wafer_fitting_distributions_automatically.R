setwd("//media//kswada//MyFiles//R//wafer")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wafer
# ------------------------------------------------------------------------------

data(wafer, package="faraway")

str(wafer)

car::some(wafer)



# ------------------------------------------------------------------------------
# fitting automatically continuous distributions in the positive real line
# ------------------------------------------------------------------------------
 
# Normal distribution model is fitted first to start the process
m1 <- gamlssML(wafer$resist, family = NO)


t1 <- chooseDist(m1, type = "realplus", trace = TRUE)


t1



# ----------
# ordering of a specified column according to the relevant GAIC
getOrder(t1, 1)[1:10]
getOrder(t1, 2)[1:10]
getOrder(t1, 3)[1:10]



# ----------
# plot the best IGAMMA model and LOGNO
histDist(wafer$resist, family = "IGAMMA", nbins = 30, line.wd = 2.5)
histDist(wafer$resist, family = "LOGNO", nbins = 30, line.wd = 2.5)



# ----------
mf <- update(m1, family = "IGAMMA")
# mf <- update(m1, family = "LOGNO")

summary(mf)



# ----------
# The value of the fitted parameters (after link function is applied)
# can also be obtained

fitted(mf, "mu")[1]
fitted(mf, "sigma")[1]



# ------------------------------------------------------------------------------
# Residual diagnostics
# ------------------------------------------------------------------------------

plot(mf)



# -->
# Note that kurtosis is almost 1.89



# ----------
wp(mf)

# wp(mf, ylim.all = 1.0)

