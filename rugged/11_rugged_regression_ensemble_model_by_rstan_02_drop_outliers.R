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
# drop outliers
#   - One of the African countries in that example, Seychelles, is far outside the cloud of other nations, being a rare country with both relatively high GDP
#     and high ruggedness. Seychelles is also unusual, in that it is a group of islands far from the coast of mainland Africa, and its main economic activitiy is tourism.
# ------------------------------------------------------------------------------
# fit model on data without Seychelles

d.ns <- d[d$country!='Seychelles',]


#slope.sigma <- 10
mod.ns <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged.c + bA * cont_africa + rugged.c * cont_africa*bAR,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, slope.sigma),
    bA ~ dnorm(0, slope.sigma),
    bAR ~ dnorm(0, slope.sigma),
    sigma ~ dunif(0, 10)
  ),data = d.ns
) 


precis(mod.full, digits = 3)


precis(mod.ns, digits = 3)


coeftab(mod.full, mod.ns)



# ----------
mod.ns.af <- predict_mu(mod.ns, d.predict.af)

mod.ns.naf <- predict_mu(mod.ns, d.predict.naf)



# ----------
par(mfrow=c(1,2))

plot_model_mu(d.ns[d.ns$cont_africa==1,], d.predict.af,  mod.ns.af, 'Africa, no Seychelles')

plot_model_mu(d.ns[d.ns$cont_africa==0,], d.predict.naf,  mod.ns.naf, 'not Africa, no Seychelles')



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
# compare(mod.full, mod.ns)

# --> nbs is different


# ----------
par(mfrow=c(1,1))

# plot Africa's only predictions

plot_model_mu(d[d$cont_africa==1,], d.predict.af,  mod.full.af, 'Africa full (red+grey) vs Africa without Seychelles(yellow+green)')

lines(d.predict.af$rugged.c, mod.ns.af$mean, col='yellow')

shade(mod.ns.af$pi, d.predict.af$rugged.c, col=col.alpha('green'))



# ----------
# compare distributions of slope for Africa countries in two models
mod.full.sample <- extract.samples(mod.full)

mod.ns.sample <- extract.samples(mod.ns)

mod.full.sample <- mutate(mod.full.sample, af_slope=bR+bAR)

mod.ns.sample <- mutate(mod.ns.sample, af_slope=bR+bAR)



# ----------
summary(mod.full.sample$af_slope)

mean(mod.full.sample$af_slope>0)



# ----------
summary(mod.ns.sample$af_slope)

mean(mod.ns.sample$af_slope>0)



# ----------
summary(mod.full.sample$bR)

summary(mod.ns.sample$bR)


# ----------
dens(mod.full.sample$af_slope, xlim=c(-0.3, 0.5),   col='blue')

dens(mod.ns.sample$af_slope, add=T, col='red')

dens(mod.full.sample$af_slope-mod.ns.sample$af_slope, add=T, col='black')#not sure it's a correct procedure



# -->
# Without Seychelles data point slope for African countries reduces almost twice but is still positive compared to non-African countries that have a negative slope.
# The use of strong regularised priors with sigma = 0.1 decreases all slopes and make differences between full and NoSeychelles(NS) models almost neglectable.
# Even for NS model there are 0.77 probability that slope for African countries is greater than zero.
