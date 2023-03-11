setwd("//media//kswada//MyFiles//R//rugged")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rugged
# ------------------------------------------------------------------------------
data("rugged", package = "rethinking")

d <- rugged

dim(d)

str(d)



# ----------
# make log version of outcome
d$log_gdp <- log(d$rgdppc_2000)


# extract countries with GDP data
d <- d[complete.cases(d$rgdppc_2000), ]


d$rugged.c <- d$rugged - mean(d$rugged) 




# ------------------------------------------------------------------------------
# regression modeling with interactions by map()
# ------------------------------------------------------------------------------

slope.sigma <- 10
#(!) try to make several runs varying this value(e.g. 0.1 correspondts to regularized priors and difference between full and ns model becomes smaller)

mod.full <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged.c + bA * cont_africa + rugged.c * cont_africa * bAR,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, slope.sigma),
    bA ~ dnorm(0, slope.sigma),
    bAR ~ dnorm(0, slope.sigma),
    sigma ~ dunif(0, 10)
  ), data=d
)


precis(mod.full, digits = 3)



# ----------
# plot predictions for Africa and not Africa
#### helper functions ####
predict_mu <- function(model, d.predict){
  mu <- link(model, data=d.predict)
  mu.mean <- apply(mu, 2, mean)
  mu.pi <- apply(mu, 2, PI)
  list(mean=mu.mean, pi=mu.pi)  
}


plot_model_mu <- function(d.raw, d.predict, mu.pred, title){
  plot(log_gdp ~ rugged.c, data=d.raw, col='blue', ylim=c(5,12), xlim=c(-2, 6))
  lines(d.predict$rugged.c, mu.pred$mean, col='red')
  shade(mu.pred$pi, d.predict$rugged.c)
  mtext(title)
}



d.predict.af <- data.frame(rugged.c=seq(-2,6,by=0.1), cont_africa=1)

d.predict.naf <- data.frame(rugged.c=seq(-2,6,by=0.1), cont_africa=0)

mu.full.af <- predict_mu(mod.full, d.predict.af)

mu.full.naf <- predict_mu(mod.full, d.predict.naf)



# ----------
par(mfrow=c(1,2))

plot_model_mu(d[d$cont_africa==1,], d.predict.af,  mu.full.af, 'Africa, full')

plot_model_mu(d[d$cont_africa==0,], d.predict.naf,  mu.full.naf, 'not Africa, full')

