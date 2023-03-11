
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tempb
#  - Daily minimum and maximum temperatures from the weather station in Badajoz, Spain
#    from 1 January 1955 to 31 December 2015.
---------------------------------------------------------------------------

  
data(tempb, package = "ks")


str(tempb)




# ------------------------------------------------------------------------------
# kernel density estimation
# ------------------------------------------------------------------------------


library(ks)


# compute a density estimate with a default plug-in bandwidth
# this is large data set, binned approximation are invoked in the density estimate and bandwidth

fhat <- kde(tempb[,c("tmin", "tmax")])




# ----------
# bandwidth matrix

fhat$H


# ----------
# estimate
dim(fhat$estimate)



# ---------
# contour plot and perspective plot

plot(fhat, display = "filled.contour")


plot(fhat, display = "persp")



# ----------
# modal region

plot(fhat, cont = 50)



# ----------
# density support estimate
plot(fhat, cont = 99.95)



