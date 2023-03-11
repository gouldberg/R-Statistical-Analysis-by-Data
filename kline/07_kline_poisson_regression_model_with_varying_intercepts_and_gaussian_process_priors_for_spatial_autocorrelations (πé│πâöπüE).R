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
d$has_high_contact <- ifelse( d$contact=="high" , 1 , 0 )



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
# Various models
# ------------------------------------------------------------------------------

# model with the Gaussian process
mod.gp <- map2stan(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + g[society] + bp*logpop,
    g[society] ~ GPL2( Dmat, etasq, rhosq, 0.01 ),
    a ~ dnorm(0,10),
    bp ~ dnorm(0,1),
    etasq ~ dcauchy(0,1),
    rhosq ~ dcauchy(0,1)
  ),
  data=list(
    total_tools=d$total_tools,
    logpop=d$logpop,
    society=d$society,
    Dmat=islandsDistMatrix),
  warmup=2000, iter=1e4 , chains=3, cores=10)


precis(mod.gp, depth=2)



# ----------
# fixed slope and intercept
mod.fixed <- map2stan(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bp*logpop + bc*has_high_contact,
    a ~ dnorm(0, 10),
    c(bp, bc) ~ dnorm(0, 1)
  ), 
  data=d, warmup=500, iter=3000 , chains=3, cores=10
)


precis(mod.fixed, depth=2)



# ----------
# model with varying intercept
mod.varintercept <- map2stan(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + a_society[society] + bp*logpop,
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 1),
    a_society[society] ~ dnorm(0, sigma_society),
    sigma_society ~ dcauchy(0, 1)
  ),
  data=d, iter=4000, chains=3, cores=10
)


precis(mod.varintercept, depth=2)



# ----------
# model with varying intercept and slope
mod.var.intercept.slope <- map2stan(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + a_society[society] + bp_society[society]*logpop,
    a ~ dnorm(0, 10),
    c(a_society, bp_society)[society] ~ dmvnormNC(sigma_society, Rho),
    sigma_society ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data=d, warmup=2000, iter=1e4, chains=3, cores=10
)


precis(mod.var.intercept.slope, depth=2)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
(cmp <- compare(mod.fixed, mod.varintercept, mod.var.intercept.slope, mod.gp))


plot(cmp)


# Summary: the best model is GP model, it has the smallest WAIC and the smallest number of effective parameters
# It is exciting that effective number of parameters for m13m4.gp is 3.9 that is less than 4.3 of "all fixed" model m13m4.fixed
# The actual number of parameters that are estimated for those models are 14 and 3 correspondingly.
# Model with pooled varying intercept and slope is ranked second by WAIC and has 4.8 effective params. 
# It has a weight equal to 0.21 in model scoring.
