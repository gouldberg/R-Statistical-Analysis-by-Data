setwd("//media//kswada//MyFiles//R//kline")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Kline2
#   - This is the ordinary data with coordinats
# ------------------------------------------------------------------------------
data("Kline2", package = "rethinking")

d <- Kline2

dim(d)
str(d)

d$society <- 1:10



# ------------------------------------------------------------------------------
# data:  islandDistMatrix
#   - This is the geographic distance matrix, measured in 1000 kilometers
#   - We will use these distances as a measure of similarity in technology exposure.
#   - This will allow us to estimate varying intercepts for each society that account for non-independence in tools as a function of their geographical similarly.
# ------------------------------------------------------------------------------
data("islandsDistMatrix", package = "rethinking")

islandsDistMatrix

# Dmat <- islandsDistMatrix
# colnames(Dmat) <- c("Ml", "Ti", "SC", "Ya", "Fi", "Tr", "Ch", "Mn", "To", "Ha")



# ------------------------------------------------------------------------------
# poisson regresion with varying intercepts (a_society), to handle overdispersion
# ------------------------------------------------------------------------------

mod3 <- map2stan(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + g[society] + bp * logpop,
    g[society] ~ GPL2(Dmat, etasq, rhosq, 0.01),
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 1),
    etasq ~ dcauchy(0, 1),
    rhosq ~ dcauchy(0, 1)
  ),
  data = list(
    total_tools = d$total_tools,
    logpop = d$logpop,
    society = d$society,
    Dmat = islandsDistMatrix),
  iter = 1e4, chains = 4, warmup = 2000, cores = 10
)


stancode(mod3)


plot(mod3)


precis(mod3, digits = 3, depth = 2)



# -->
# the coef for log population, bp, is very much as it was before we added all this Gaussian process stuff.
# This suggests that it's hard to explain all of the asoociation between tool counts and population as a side effect of geographic contact.

# g parameters are the Gaussian process varying intercepts for each society.
# Like a and bp, they are on the log-count scale, so they are hard to interpret raw.

# rhosq is quite skewed, the mean is not even inside 89% HPDI.



# ------------------------------------------------------------------------------
# model inspection
#  - posterior distribution of the spatial covariance between pairs of societies
#    (joint posterior distribution of etasq and rhosq, which defines a posterior distribution of covariance function)
# ------------------------------------------------------------------------------
post <- extract.samples(mod3)

par(mfrow=c(1,1))
curve(median(post$etasq) * exp(-median(post$rhosq) * x^2), from = 0, to = 10, xlab = "distance(1000 km)", ylab = "covariance", ylim=c(0,1), yaxp=c(0, 1, 4), lwd = 2)
for(i in 1:100) curve(median(post$etasq[i]) * exp(-median(post$rhosq[i]) * x^2), add=TRUE, col = col.alpha("black", 0.2))


# -->
# Curves that peak at twice the posterior median peak, around 0.2, are commonplace.
# And curves that peak at half the median are very common, as well.

# There's a lot of uncertainty about how strong the spatial effect is, but the majority of posterior curves decline to zero covariance before 4000 kilometers.



# ------------------------------------------------------------------------------
# model inspection
#  - posterior median covariance among societies
# ------------------------------------------------------------------------------
# compute posterior median covariance among societies
K <- matrix(0, nrow = 10, ncol = 10)

for(i in 1:10){
  for(j in 1:10){
    K[i,j] <- median(post$etasq) * exp(-median(post$rhosq) * islandsDistMatrix[i,j]^2)
    diag(K) <- median(post$etasq) + 0.01
  }
}


# ----------
# convert to correlation matrix
Rho <- round(cov2cor(K), 2)
colnames(Rho) <- c("Ml", "Ti", "SC", "Ya", "Fi", "Tr", "Ch", "Mn", "To", "Ha")
rownames(Rho) <- colnames(Rho)

Rho


# -->
# The cluster of small societies in the upper-left of the matrix (Ml, Ti, SC) are highly correlated, all above 0.8
# These societies are very close together, and they also have similar tool totals.

# On the other end of spectrum is Hawaii (Ha), which is so far from all of other societies that the correlation decays to zero everyplace.


# ------------------------------------------------------------------------------
# model inspection
#  - plot posterior median correlation among societies in geographic space, and against relationship between total tools and log population
# ------------------------------------------------------------------------------

# scale point size to logpop
psize <- exp(d$logpop / max(d$logpop) * 1.5) - 2


# compute posterior median relationship, ignoring distance
logpop.seq <- seq(from = 6, to = 14, length.out = 30)
lambda <- sapply(logpop.seq, function(lp) exp(post$a + post$bp * lp))
lambda.median <- apply(lambda, 2, median)
lambda.PI80 <- apply(lambda, 2, PI, prob = 0.8)



# ----------
par(mfrow=c(1,2))

plot(d$lon2, d$lat, xlab = "longitude", ylab = "latitude", col = rangi2, cex = psize, pch = 16, xlim = c(-50, 30))
labels <- as.character(d$culture)
text(d$lon2, d$lat, labels = labels, cex = 0.7, pos = c(2,4,3,3,4,1,3,2,4,2))

for(i in 1:10){
  for(j in 1:10){
    if(i < j){
      lines(c(d$lon2[i], d$lon2[j]), c(d$lat[i], d$lat[j]), lwd = 2, col = col.alpha("black", Rho[i,j]^2))
    }
  }
}


plot(d$logpop, d$total_tools, xlab = "log population", ylab = "total tools", col = rangi2, cex = psize, pch = 16)
labels <- as.character(d$culture)
text(d$logpop, d$total_tools, labels = labels, cex = 0.7, pos = c(4,3,4,2,2,1,4,4,4,2))
lines(logpop.seq, lambda.median, lty = 2)
lines(logpop.seq, lambda.PI80[1,], lty = 2)
lines(logpop.seq, lambda.PI80[2,], lty = 2)

for(i in 1:10){
  for(j in 1:10){
    if(i < j){
      lines(c(d$logpop[i], d$logpop[j]), c(d$total_tools[i], d$total_tools[j]), lwd = 2, col = col.alpha("black", Rho[i,j]^2))
    }
  }
}


# Darker lines: indicate stronger correlations, with pure white being zero correlation and pure black 100% correlation
# Close societies have stronger correlations

# But Malekula, Tikopia, Santa Cruz are below the expected number of tools for thier populations

# Similarly, Manus and the Trobriands are geographically close, have a substantial posterior correlation, and fewer tools than expected for theire population sizes.
# Tonga has more tools than expected for its population, and its proximity to Fiji counteracts some of the tug Fiji's smaller neighbors (Ml, Ti, SC) exrert on it.
# So the model seems to think Fiji would have fewer tools, if it were not for Tonga.
