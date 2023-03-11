setwd("//media//kswada//MyFiles//R//lynx")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lynx
# ------------------------------------------------------------------------------

data(lynx, package = "datasets")


str(lynx)


lynx



# ----------
lynxl <- log10(lynx)

dlynxl <- diff(log10(lynx))





# ------------------------------------------------------------------------------
# test of non-linearity via locally linear forecasts
#    - An interesting tool for inspecting possible nonlinearities in the time series is
#      the locally linear autoregressive fit plot
#      Suppose you think that the dynamical system underlying your time series is best reconstructed
#      with embedding dimension m and time delay d.
#      Then the locally linear autoregressive fit plot displays the relative error made by forecasting time series
#      values with linear models of the form:
#      x(t+s) = phi0 + phi1 * X(t) + ... + phim * X(t|(m|1)d)
#      estimated on points in the sphere of radius eps around xm(t) for a range of values of eps.
#      A minimum attained at relatively small values of eps may indicate that a global linear model
#      would be inappropriate for the approximation of the time series dynamics.
# ------------------------------------------------------------------------------

library(tsDyn)



# locally AR(3) model fitting
obj <- llar(lynxl, m = 3)


obj



# ----------
# relative errors
# eps:  vector of neighbourhood sizes (in the same order of RMSE)
plot(obj)



# RMSE
plot(RMSE ~ eps, data = obj, type = "l", log = "x")




# ----------
# llar.predict:  tries to extend the given time series by n.ahead points by iteratively fitting locally
# (in the embedding space of dimension m and time delay d) a linear model.
# If the spatial neighbourhood window is too small, your time series last point would be probably isolated.
# You can ask to automatically enlarge the window eps by a factor of r% sequentially,
# until enough neighbours are found for fitting the linear model.

x.new <- llar.predict(lynxl, n.ahead = 20, m = 3, eps = 1, onvoid = "enlarge", r = 5)


lag.plot(x.new)




# ----------
# llar.fitted:  gives out-of-sample fitted values from locally linear models

m <- 3

x.fitted <- llar.fitted(lynxl, m = m, eps = 1)

lag.plot(x.fitted)


par(mfrow = c(1,1))

plot(lynxl, type = "l")

lines(ts(x.fitted, start = 1821+m, end = 1934+m), col = "blue", lty = 2)




# ------------------------------------------------------------------------------
# Space-time separation plot to define Theiler Window (tw)
#   - To avoid temporal nearness, the distance between pairs of points has to be greater than some time w,
#     called Theiler Window.
#   - Space-time separation plot method to estimate the Theiler Window.
#     The number of pairs of points is computed as a function of the spatial separation and the separation in time.
#     The result is displayed as contours of constant probability for the distribution of the spatial separation
#     as a function of the time separation
# ------------------------------------------------------------------------------


library(nonlinearTseries)


# numberPercentages: number of contour lines to be computed
# max.radius: maximum radius of the sphere in which pairs of points are looked for.

spaceTimePlot(time.series = lynxl, embedding.dim = 3, time.lag = 10,
              time.step = 1, max.radius = 5, numberPercentages = 10, do.plot = TRUE,
              main = "", xlab = "Separation in time", ylab = "Separation in space")





# ------------------------------------------------------------------------------
# Percentage of false nearest neighbours as a function of the embedding dimension to define embedding dimension (m)
# ------------------------------------------------------------------------------


# embedding dimensions: from 1 to m_max
m.max <- 10


# tentative time delay
d <- 3


# Theiler window
tw <- 10


# escape factor: factor for escaping from the neighbourhood
rt <- 20


# neighbourhood diameter:  all the points inside this ball are considered neighbours,
# no matter whether they are real or false
eps <- sd(lynxl) / 10



# ----------
fn <- false.nearest(lynxl, m.max, d, tw, rt, eps)


fn



# ----------
graphics.off()
par(mfrow = c(1,1))
plot(fn)



# -->
# m = 2 is the first embedding dimension that gives a small percentage of false nearest neighbours.




# ------------------------------------------------------------------------------
# Average Mutual Information to define time delay (d)
# ------------------------------------------------------------------------------


# average mutual information (AMI) to suggest time delay (d)
# Mutual infromation is a measure of how much the knowledge of X(Y) reduces uncertainty about Y(X)
# In other words, mutual information describes the information that the measurement s(t) at time t brings
# to the measurement s(t + d) at time t + d
# s(t):  for example histogram

# In the series, the mutual information is averaged over all data s(t) to yield the AMI.
# If time delay (d) is chosen as the value around the 1st minimum of the AMI, then Y(t) and Y(t+d)
# are partially but not totally independent

library(tseriesChaos)


mutual(lynxl, lag.max = 20)



# -->
# 1st minimum average mutual information at lag 3 (time delay = 3)



# ------------------------------------------------------------------------------
# testing the conditional mutual independence and linearity
# ------------------------------------------------------------------------------

library(tsDyn)



# m: vectpr pf embedding dimensions
# d: time delay

delta.test(lynxl, m = 2:6)



# delta test of linearity based on conditional mutual information
delta.lin.test(lynxl)



# -->
# We reject conditional independence quite easily.
# There is some trouble instead for deciding to reject or not linearity. 




# ------------------------------------------------------------------------------
# Recurrence plot:  Thresholded Recurrence plot
# ------------------------------------------------------------------------------


n <- length(lynxl)

x <- lynxl



eps <- 10

distx <- matrix(, n, n)



graphics.off()

par(mfrow = c(1,1))

plot(0, 0, type = "n", xlab = "i", ylab = "j", xlim = c(0, n), ylim = c(0, n), cex.lab = 1.7, cex.axis = 1.3)


# IT TAKES TIME !!!
for(i in 1:n){
  for(j in 1:n){
    distx[i,j] <- sqrt((x[i] - x[j])^2)
#    distx[i,j] <- sqrt((x[i] - x[j])^2 + (y[i] - y[j])^2)
    
    if(distx[i,j] < eps) points(i, j, pch = 19, cex = 0.01)
    
  }
}




# ------------------------------------------------------------------------------
# Recurrence plot:  Un-Thresholded Recurrence plot
# ------------------------------------------------------------------------------

time <- seq(1:n)

library(gplots)


# normalized distances matrix previously computed
distx <- distx / max(distx)


distx <- 1 - distx


graphics.off()

par(mfrow = c(1,1))

filled.contour(time, time, distx, col = gray.colors(10, start = 1, end = 0), nlevels = 10,
               xlab = "i", ylab = "j", main = "", xlim = c(0, n), ylim = c(0, n), las = 0,
               key.axes = axis(4, las = 1))




# ------------------------------------------------------------------------------
# Recurrence plot
#   - Recurrence Plot shows how the trajectory repeats itself,
#     and that it returns to visit the states already visited in the past, within the margin of a tolerance eps.
#   - Note that an recurrence plot is a 2-dimensional representation of a trajectory in a higher-dimensional phase space
# ------------------------------------------------------------------------------


recurr(lynxl, m = 2, d = 3, levels = c(0,0.2,0.4,0.6,1))


plot(lynxl, type = "l")




# ------------------------------------------------------------------------------
# Principal Component Analysis for Embedding  (useful for noizy data)
# ------------------------------------------------------------------------------

m <- 10

d <- 3


emb <- embedd(lynxl, m, d)



# ----------
pc <- prcomp(emb, center = TRUE, scale. = TRUE)


summary(pc)



sd <- pc$sdev


var <- sd^2

( var.percent <- var/sum(var) * 100 )


barplot(var.percent)



# ----------
xyz2 <- predict(pc)[,1:2]


library(scatterplot3d)

scatterplot3d(xyz2, type = "l", cex.lab = 1.5, cex.axis = 1.2, lab = c(4,4,7), lab.z = 4)




# ------------------------------------------------------------------------------
# Singular Spctral Analaysis matrix decomposition and grouping diagnostics
# Run grouping diagnostics to group eigentriplets
# ------------------------------------------------------------------------------

# SSA Decomposition

library(Rssa)


# input SSA window length L
s <- ssa(lynxl, 50, kind = "toeplitz-ss")



graphics.off()


# 1st visual diagnostics:  Eigenspectrum

# plot 1st 20 largest
eigenvalues <- plot(s, numvalues = 20, col = "black", lwd = 2)

eigenvalues



# ----------
# 2nd visual diagnostic:  Eigenvector plots

plot(s, type = "vectors", lwd = 2)




# ----------
# 3rd visual diagnostic:  Pairwise scatterplots of eigenvectors

# plot 1st 9 pairs
plot(s, type = "paired", idx = 1:10, plot.contrib = FALSE, col = "black", lwd = 2)




# ----------
# weighted correlation matrix

# 1st 20 eigentriplets
plot(w <- wcor(s, groups = c(1:20)))


# table for 1st 10 eigentriplets
( w.corr.res <- wcor(s, groups = c(1:10)) )


