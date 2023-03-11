
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Lorenz Attractor
# ------------------------------------------------------------------------------

# Lorenz Equation
# dx / dt = sigma * (y - x)
# dy / dt = x * (rho - z) - y
# dz / dt = x * y - beta * z



# parameters: sigma, beta, rho
parms <- c(10, 8/3, 28)

tinit <- 0

tfin <- 100


step <- 0.01

times <- seq(tinit, tfin, by = step)



# ----------
# system equations

funct <- function(t, integ, p){
  
  x <- integ[1]

  y <- integ[2]
  
  z <- integ[3]
  
  sigma <- parms[1]
  
  beta <- parms[2]
  
  rho <- parms[3]
  
  dx <- sigma * (y - x)
  
  dy <- x * (rho - z) - y
  
  dz <- x * y - beta * z
  
  list(c(dx, dy, dz))
}



# ----------

library(deSolve)


cinit <- c(1,1,1)


xyz <- lsoda(cinit, times, funct, parms)

head(xyz)



# ----------
# phase space portrait
graphics.off()


par(mfrow = c(3,1))


plot(xyz[,1], xyz[,2], type = "l", xlab = "t", ylab = "x(t)", xlim = c(xinit, tfin), ylim = c(-30, 30))

plot(xyz[,1], xyz[,3], type = "l", xlab = "t", ylab = "y(t)", xlim = c(xinit, tfin), ylim = c(-30, 30))

plot(xyz[,1], xyz[,4], type = "l", xlab = "t", ylab = "z(t)", xlim = c(xinit, tfin), ylim = c(0, 50))


library(scatterplot3d)

par(mfrow = c(1,1))

scatterplot3d(xyz[,2], xyz[,3], xyz[,4], type = "l", xlim = c(-30, 30), cex.lab = 1.4, cex.axis = 1.2)




# ------------------------------------------------------------------------------
# Lorenz system: invariant density, 1 trajectory
# ------------------------------------------------------------------------------


library(tseriesChaos)

lorenz.syst


a <- 10

b <- 28

c <- -8/3

x0 <- 1

y0 <- 1

z0 <- 1


t_init <- 0

t_fin <- 1000


step.int <- 0.01


# directly integrate
x <- sim.cont(lorenz.syst, start = t_init, end = t_fin, dt = step.int,
              start.x = c(x0, y0, z0), parms = c(a, b, c), obs.fun = function(x) x[1])

y <- sim.cont(lorenz.syst, start = t_init, end = t_fin, dt = step.int,
              start.x = c(x0, y0, z0), parms = c(a, b, c), obs.fun = function(x) x[2])

z <- sim.cont(lorenz.syst, start = t_init, end = t_fin, dt = step.int,
              start.x = c(x0, y0, z0), parms = c(a, b, c), obs.fun = function(x) x[3])



# ----------
library(gplots)


h2d <- hist2d(x, z, show = FALSE, same.scale = F, nbins = 100)


h2d

h2d$counts <- h2d$counts / max(h2d$counts)


# invariant density of the Lorenz system
# 2-dimensional histogram of the positions (x(t), z(t)) obtained by 1000 iterations

filled.contour(h2d$x, h2d$y, h2d$counts, col = gray.colors(10, start = 0, end = 1), nlevels = 10,
               xlab = "x(t)", ylab = "z(t)", main = "", xlim = c(-20, 20), ylim = c(0, 50), las = 0,
               key.axes = axis(4, las = 1))

filled.contour(h2d$x, h2d$y, h2d$counts, col = topo.colors(10), nlevels = 10,
               xlab = "x(t)", ylab = "z(t)", main = "", xlim = c(-20, 20), ylim = c(0, 50), las = 0,
               key.axes = axis(4, las = 1))




# ------------------------------------------------------------------------------
# Remove transient part
# ------------------------------------------------------------------------------

# integration time step considered as transient
trans <- 2000



# ----------
# discard initial transient

x <- window(xyz[,2], trans)

y <- window(xyz[,3], trans)

z <- window(xyz[,4], trans)


t_start <- trans * step - step

t_time <- seq(t_start, tfin, by = step)



# ----------
par(mfrow = c(3,1))


plot(t_time, x, type = "l", xlab = "t", ylab = "x(t)", xlim = c(t_start, tfin), ylim = c(-30, 30))

plot(t_time, y, type = "l", xlab = "t", ylab = "x(t)", xlim = c(t_start, tfin), ylim = c(-30, 30))

plot(t_time, z, type = "l", xlab = "t", ylab = "x(t)", xlim = c(t_start, tfin), ylim = c(0, 50))




# ------------------------------------------------------------------------------
# Percentage of false nearest neighbours as a function of the embedding dimension
# ------------------------------------------------------------------------------


# embedding dimensions: from 1 to m_max
m.max <- 6


# tentative time delay
d <- 18


# Theiler window
tw <- 100


# escape factor: factor for escaping from the neighbourhood
rt <- 10


# neighbourhood diameter:  all the points inside this ball are considered neighbours,
# no matter whether they are real or false
eps <- sd(x) / 10



# ----------
fn <- false.nearest(x, m.max, d, tw, rt, eps)


fn



# ----------
graphics.off()
par(mfrow = c(1,1))
plot(fn)



# -->
# m = 3 is the first embedding dimension that gieves a small percentage of false nearest neighbours.




# ------------------------------------------------------------------------------
# Embedding Time Delay
# ------------------------------------------------------------------------------

# largest lag
lm <- 60


# average mutual information (AMI) to suggest time delay (d)
# Mutual infromation is a measure of how much the knowledge of X(Y) reduces uncertainty about Y(X)
# In other words, mutual information describes the information that the measurement s(t) at time t brings
# to the measurement s(t + d) at time t + d
# s(t):  for example histogram

# In the series, the mutual information is averaged over all data s(t) to yield the AMI.
# If time delay (d) is chosen as the value around the 1st minimum of the AMI, then Y(t) and Y(t+d)
# are partially but not totally independent

mutual(x, lag.max = lm)



# -->
# 1st minimum AMI is for a lag = 18



# ------------------------------------------------------------------------------
# Embedding
# ------------------------------------------------------------------------------


m <- 3

d <- 18

xyz <- embedd(x, m, d)

scatterplot3d(xyz, type = "l")



# ----------
# try d = 10
# "aesthetically" more satisfactory
xyz <- embedd(x, 3, 10)

scatterplot3d(xyz, type = "l")



# ----------
# try d = 2
xyz <- embedd(x, 3, 2)

scatterplot3d(xyz, type = "l")



# ----------
# try d = 100
xyz <- embedd(x, 3, 100)

scatterplot3d(xyz, type = "l")




# ------------------------------------------------------------------------------
# Space-time separation plot to define Theiler Window
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

spaceTimePlot(time.series = x, embedding.dim = 3, time.lag = 10,
              time.step = 1, max.radius = 400, numberPercentages = 10, do.plot = TRUE,
              main = "", xlab = "Separation in time", ylab = "Separation in space")



# -->
# Theiler Window = 100, since after this value almost-regular oscillaitions begin to appear.




# ------------------------------------------------------------------------------
# Principal Component Analysis for Embedding  (useful for noizy data)
# ------------------------------------------------------------------------------

parms <- c(10, 8/3, 28)

tinit <- 0

tfin <- 100


step <- 0.01

times <- seq(tinit, tfin, by = step)


cinit <- c(1,1,1)

xyz <- lsoda(cinit, times, funct, parms)


trans <- 2000

x <- window(xyz[,2], trans)

t_start <- trans * step - step

t_time <- seq(t_start, tfin, by = step)



# ----------
m <- 11

d <- 1


emb <- embedd(x, m, d)



# ----------
pc <- prcomp(emb, center = TRUE, scale. = TRUE)


summary(pc)



sd <- pc$sdev


var <- sd^2

( var.percent <- var/sum(var) * 100 )


barplot(var.percent)



# ----------
xyz2 <- predict(pc)[,1:3]


scatterplot3d(xyz2, type = "l", cex.lab = 1.5, cex.axis = 1.2, lab = c(4,4,7), lab.z = 4)




