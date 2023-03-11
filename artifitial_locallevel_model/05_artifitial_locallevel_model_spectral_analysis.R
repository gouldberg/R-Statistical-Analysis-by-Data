rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Artifitial local level model
# ------------------------------------------------------------------------------

# generate local level model data


set.seed(23)


library(dlm)



# ----------
# set model
W <- 1

V <- 2

m0 <- 10

C0 <- 9

mod <- dlmModPoly(order = 1, dW = W, dV = V, m0 = m0, C0 = C0)



# ----------
# generate observation by kalman forecast

t_max <- 200

sim_data <- dlmForecast(mod = mod, nAhead = t_max, sampleNew = 1)

y <- ts(as.vector(sim_data$newObs[[1]]))

plot(y, ylab = "y")




# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
# ------------------------------------------------------------------------------

length(y)

nextn(length(y))




# ----------
par(mfrow=c(2,1))


# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
y.per <- astsa::mvspec(y, log = "no")

y2.per <- astsa::mvspec(diff(y), log = "no")



# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

( ker <- kernel("modified.daniell", c(3,3)) )

plot(ker)


# ----------
# Lh = 1 / sum(hk^2)  k = -6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6  --> 9.24, which is close to the value of L = 9
1 / sum(ker$coef[c(6,5,4,3,2,1,2,3,4,5,6)]^2)



# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.25:  tapering the upper and lower 25% of the data

graphics.off()
par(mfrow=c(1,1))

y.smo <- astsa::mvspec(y, kernel = ker, taper = 0.25, log = "no")
