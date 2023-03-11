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
dd <- d[complete.cases(d$rgdppc_2000), ]


# split countries into Africa and not-Africa
d.A1 <- dd[dd$cont_africa == 1, ]
d.A0 <- dd[dd$cont_africa == 0, ]


summary(d.A1$rugged)
summary(d.A0$rugged)



# ------------------------------------------------------------------------------
# plot posterior distribution
# ------------------------------------------------------------------------------

# plot posterior distribution for model 5 --> The slope reverses direction inside and outtside of Africa.

rugged.eq <- seq(from = -1, to = 8, by = 0.25)

mu.NotAfrica <- link(mod5, data = data.frame(cont_africa = 0, rugged = rugged.seq))

mu.Africa <- link(mod5, data = data.frame(cont_africa = 1, rugged = rugged.seq))



# ----------
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)

mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI, prob = 0.97)

mu.Africa.mean <- apply(mu.Africa, 2, mean)

mu.Africa.PI <- apply(mu.Africa, 2, PI, prob = 0.97)



# ----------
par(mfrow=c(1,2))

plot(log(rgdppc_2000) ~ rugged, data = d.A1, col = rangi2, ylab = "log GDP year 2000", xlab = "Terrain Ruggedness Index")

mtext("African nations", 3)

lines(rugged.seq, mu.Africa.mean, col = rangi2)

shade(mu.Africa.PI, rugged.seq, col = col.alpha(rangi2, 0.3))


plot(log(rgdppc_2000) ~ rugged, data = d.A0, col = "black", ylab = "log GDP year 2000", xlab = "Terrain Ruggedness Index")

mtext("Non-African nations", 3)

lines(rugged.seq, mu.NotAfrica.mean, col = rangi2)

shade(mu.NotAfrica.PI, rugged.seq)



# ------------------------------------------------------------------------------
# Compute gamma's posterior distribution
# ------------------------------------------------------------------------------

# Since gamma does not appear in this table (it was not estimated), we have to comput it ourselves.
precis(mod5, corr = TRUE, digits = 3)



# ----------
post <- extract.samples(mod5)

gamma.Africa <- post$bR + post$bAR * 1

gamma.notAfrica <- post$bR + post$bAR * 0



# ----------
# Nearly identical to the MAP values, of course.
mean(gamma.Africa)

mean(gamma.notAfrica)



# ----------
par(mfrow=c(1,1))

dens(gamma.Africa, xlim = c(-0.5, 0.6), ylim = c(0, 5.5), xlab = "gamma", col = rangi2)

dens(gamma.notAfrica, add = TRUE)



# ----------
# The proportion of the differences is below zero
diff <- gamma.Africa - gamma.notAfrica

sum(diff < 0) / length(diff)



# -->
# It is highly implausible that the slope association ruggedness with log-GDP is lower inside Africa than outside it.
# Note that the calculation above is the distribution of the difference between the two.
# The distribution of their difference is not the same as the visual overlap of their marginal distributions.
