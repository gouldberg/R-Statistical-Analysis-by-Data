setwd("//media//kswada//MyFiles//R")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Negative Binomial distribution for count data
#   - The mean and variance of Y of a negative binomial GLM is:  E(Y) = mu, var(Y) = mu + mu^2 / k
#     If k is large relative to mu^2, the term mu^2 / k is close to zero, and the variance of Y is mu.
#     In such cases the negative binomial converges to the Poisson distribution
#   - 2 way parametrizations for var(Y) = mu + mu^2 / k, or mu + alpha * mu^2, depending on commercial software packages.
#     When using glm.nb as the (inverse) dispersion parameter k becomes larger than 0, variability in the data decreases.
#     For functions using direct parametrization, greater values of the dispersion parameter alpha indicate that there is more variability in the data.
# ------------------------------------------------------------------------------

# dnbinom is in direct parametrization by k:  get probability Pr(Y=0 | mu = 3, k = 1)
dnbinom(0, mu=3, size=1)


library(stats)

mu1B=3 ; k1B=0.1
mu1C=3 ; k1C=1
mu1D=3 ; k1D=1000

mu2B=15 ; k2B=0.1
mu2C=15 ; k2C=1
mu2D=15 ; k2D=1000

mu3B=50 ; k3B=0.1
mu3C=50 ; k3C=1 
mu3D=50 ; k3D=1000

x1B <- 0:10; Y12 <- dnbinom(x1B,mu=mu1B,size=k1B)
x1C <- 0:10; Y13 <- dnbinom(x1C,mu=mu1C,size=k1C)
x1D <- 0:10; Y14 <- dnbinom(x1D,mu=mu1D,size=k1D)

x2B <- 0:25; Y22 <- dnbinom(x2B,mu=mu2B,size=k2B)
x2C <- 0:25; Y23 <- dnbinom(x2C,mu=mu2C,size=k2C)
x2D <- 0:25; Y24 <- dnbinom(x2D,mu=mu2D,size=k2D)

x3B <- 0:100; Y32 <- dnbinom(x3B,mu=mu3B,size=k3B)
x3C <- 0:100; Y33 <- dnbinom(x3C,mu=mu3C,size=k3C)
x3D <- 0:100; Y34 <- dnbinom(x3D,mu=mu3D,size=k3D)

par(mfrow = c(3,3), cex.lab = 1.5, cex.main = 1.5, mar = c(5,5,2,2))
Xlab = "Y values"
Ylab = "Probabilities"
plot(x1B,Y12,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu1B,", k =",k1B,")"))
plot(x1C,Y13,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu1C,", k =",k1C,")"))
plot(x1D,Y14,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu1D,", k =",k1D,")"))

plot(x2B,Y22,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu2B,", k =",k2B,")"))
plot(x2C,Y23,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu2C,", k =",k2C,")"))
plot(x2D,Y24,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu2D,", k =",k2D,")"))

plot(x3B,Y32,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu3B,", k =",k3B,")"))
plot(x3C,Y33,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu3C,", k =",k3C,")"))
plot(x3D,Y34,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu3D,", k =",k3D,")"))



# -->
# Note that for large k values, the negative binomial distribution becomes a Poisson distribution.
# For small values of k the negative binomial distribution allows for a large number of zeros.


