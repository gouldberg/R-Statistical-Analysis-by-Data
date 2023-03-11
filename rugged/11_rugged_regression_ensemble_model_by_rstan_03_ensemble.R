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
# models without Seychelles
# ------------------------------------------------------------------------------

slope.sigma <- 10

mod.ns.r <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged.c,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, slope.sigma),
    sigma ~ dunif(0, 10)
  ),data=d.ns
) 



mod.ns.ra <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged.c + bA*cont_africa,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, slope.sigma),
    bA ~ dnorm(0, slope.sigma),
    sigma ~ dunif(0, 10)
  ),
  data=d.ns, 
  start=list(a=c(mean(d.ns$log_gdp)),bR=0,bA=0,sigma=sd(d.ns$log_gdp))
) 



mod.ns <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged.c + bA*cont_africa + rugged.c*cont_africa*bAR,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, slope.sigma),
    bA ~ dnorm(0, slope.sigma),
    bAR ~ dnorm(0, slope.sigma),
    sigma ~ dunif(0, 10)
  ),
  data=d.ns
) 



# ----------
compare(mod.ns.r, mod.ns.ra, mod.ns)



# ----------
par(mfrow=c(1,1))

plot(coeftab(mod.ns.r, mod.ns.ra, mod.ns))



# ----------
pairs(mod.ns)



# ----------
mu.ns.af <- predict_mu(mod.ns, d.predict.af)

mu.ns.naf <- predict_mu(mod.ns, d.predict.naf)

par(mfrow=c(3,2))

d.predict.af <- data.frame(rugged.c=seq(-2,6,by=0.1), cont_africa=1)

d.predict.naf <- data.frame(rugged.c=seq(-2,6,by=0.1), cont_africa=0)


for(model in list(mod.ns.r, mod.ns.ra, mod.ns)){
  mu.af <- predict_mu(model, d.predict.af)
  plot_model_mu(d.ns[d.ns$cont_africa==1,], d.predict.af,  mu.af, 'Africa, no Seychelles')
  
  m.naf <- predict_mu(model, d.predict.naf)
  plot_model_mu(d.ns[d.ns$cont_africa==0,], d.predict.naf,  m.naf, 'not Africa, no Seychelles')
}



# ------------------------------------------------------------------------------
# averaged across models (ensemble)
# ------------------------------------------------------------------------------

mu.ensemble <- ensemble(mod.ns.r, mod.ns.ra, mod.ns, data = d.predict.af)

mu.mean <- apply(X = mu.ensemble$link, MARGIN = 2, FUN = mean)

mu.PI <- apply(X = mu.ensemble$link, MARGIN = 2, FUN = PI)

plot_model_mu(d.ns[d.ns$cont_africa==1,], d.predict.af,  list(mean=mu.mean, pi=mu.PI), 'Africa, no Seychelles')

