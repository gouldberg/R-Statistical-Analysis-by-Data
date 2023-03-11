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
# Likelihood of an Inhomogeneous Poisson Process
#
#  - The expression of the likelihood can be difficult to work out for many point processes.
#    However, in the case of the inhomogeneous poisson process (IPP) or homogeneous poisson process (HPP) it has a very simple expression
#    The log-likelihood of a realization of n independent events of an IPP with intensity
#      lambda(x) = sum(log-lambda(xi)) - integral(lambd(x) dx)
#      integral(lambda(x) dx): the expected number of cases of the IPP with intensity lambda(x) in the region
#
#  - When the intensity of the point process is estimated parametrically, the likelihood can be maximised to obtain the estimates of the parameters of the model.
#    Diggle (2003) suggests a log-linear model
#      log-lambda(x) = sum(beta * z(x)), using covariates z(x) measured at location x.
# ------------------------------------------------------------------------------

library(cubature)

# log intensity model = alpha + beta1 * x1 + beta2 + x2 + beta3 * x1^2 + beta4 * x2^2 + beta5 * x1 * x2

loglambda <- function(x, alpha, beta){
  l <- alpha + sum(beta * c(x, x * x, prod(x)))
  return(l)
}


# cubature::adaptIntegrate() compute numerically the integral that appears in the expression of the likelihood
L <- function(alphabeta, x){
  l <- apply(x,1,loglambda, alpha=alphabeta[1], beta=alphabeta[-1])
  l <- sum(l)
  intL <- adaptIntegrate(lowerLimit=c(0,0), upperLimit=c(1,1), fDim=1, tol=1e-8, 
                         f=function(x, alpha=alphabeta[1], beta=alphabeta[-1]){exp(loglambda(x, alpha, beta))})
  l <- l-intL$integral
  return(l)
}



# ------------------------------------------------------------------------------
# Likelihood of an Inhomogeneous Poisson Process
# redwood full data
# ------------------------------------------------------------------------------

x_r <- as.points(redwoodfull)



# ----------
# Maximise log-likelihood
optbeta_r <- optim(par=c(log(200),0,0,0,0,0), fn=L, control=list(maxit=1000, fnscale=-1), x=x_r)

optbeta_r



# ----------
# compute lambda on grid
grd_r <- GridTopology(cellcentre.offset = c(0.005,0.005), cellsize = c(0.01, 0.01), cells.dim = c(100, 100))

lambda_r <- exp(apply(coordinates(grd_r), 1, function(X, alpha, beta){loglambda(X, alpha, beta)}, alpha=optbeta_r$par[1], beta=optbeta_r$par[-1]))


lambda_r



# ----------
parint_r <- SpatialGridDataFrame(grd_r, data=data.frame(intensity=lambda_r))

lyt_r <- list("sp.points", SpatialPoints(x_r), pch=19, col="black", cex=0.7)

print(spplot(parint_r, at=seq(0, 400, length.out=8), col.regions=colorRampPalette(gp)(7), sp.layout=list(lyt_r)))




# ------------------------------------------------------------------------------
# Likelihood of an Inhomogeneous Poisson Process
# japanesepines data
# ------------------------------------------------------------------------------

x_j <- as.points(japanesepines)



# ----------
# Maximise log-likelihood
optbeta_j <- optim(par=c(log(100),0,0,0,0,0), fn=L, control=list(maxit=1000, fnscale=-1), x=x_j)

optbeta_j



# ----------
# compute lambda on grid
grd_j <- GridTopology(cellcentre.offset = c(0.005,0.005), cellsize = c(0.01, 0.01), cells.dim = c(100, 100))

lambda_j <- exp(apply(coordinates(grd_j), 1, function(X, alpha, beta){loglambda(X, alpha, beta)}, alpha=optbeta_j$par[1], beta=optbeta_j$par[-1]))


lambda_j



# ----------
parint_j <- SpatialGridDataFrame(grd_j, data=data.frame(intensity=lambda_j))

lyt_j <- list("sp.points", SpatialPoints(x_j), pch=19, col="black", cex=0.7)

print(spplot(parint_j, at=seq(0, 200, length.out=8), col.regions=colorRampPalette(gp)(7), sp.layout=list(lyt_j)))




# ------------------------------------------------------------------------------
# Likelihood of an Inhomogeneous Poisson Process
# cells data
# ------------------------------------------------------------------------------

x_c <- as.points(cells)



# ----------
# Maximise log-likelihood
optbeta_c <- optim(par=c(log(100),0,0,0,0,0), fn=L, control=list(maxit=1000, fnscale=-1), x=x_c)

optbeta_c



# ----------
# compute lambda on grid
grd_c <- GridTopology(cellcentre.offset = c(0.005,0.005), cellsize = c(0.01, 0.01), cells.dim = c(100, 100))

lambda_c <- exp(apply(coordinates(grd_c), 1, function(X, alpha, beta){loglambda(X, alpha, beta)}, alpha=optbeta_c$par[1], beta=optbeta_c$par[-1]))


lambda_c



# ----------
parint_c <- SpatialGridDataFrame(grd_c, data=data.frame(intensity=lambda_c))

lyt_c <- list("sp.points", SpatialPoints(x_c), pch=19, col="black", cex=0.7)

print(spplot(parint_c, at=seq(0, 200, length.out=8), col.regions=colorRampPalette(gp)(7), sp.layout=list(lyt_c)))


