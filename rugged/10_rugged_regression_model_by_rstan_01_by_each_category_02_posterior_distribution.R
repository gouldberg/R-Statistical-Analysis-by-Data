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
# Plot posterior distributeions
# ------------------------------------------------------------------------------

rugged.seq <- 0:7

pred.data <- data.frame(rugged = rugged.seq)



# ----------
mu1 <- link(mod1, data = pred.data, n = 1e4)

mu1.mean <- apply(mu1, 2, mean)

mu1.PI <- apply(mu1, 2, PI)


mu2 <- link(mod2, data = pred.data, n = 1e4)

mu2.mean <- apply(mu2, 2, mean)

mu2.PI <- apply(mu2, 2, PI)



# ----------
par(mfrow=c(1,2))
plot(log_gdp ~ rugged, data = d.A1, col = rangi2)
mtext("Africa")
lines(rugged.seq, mu1.mean)
shade(mu1.PI, rugged.seq)

plot(log_gdp ~ rugged, data = d.A0, col = rangi2)
mtext("Non-Africa")
lines(rugged.seq, mu2.mean)
shade(mu2.PI, rugged.seq)



# -->
# It makes sense that ruggedness is associated with poorer countries, in most of the world.
# In theory, rugged terrain means transport is difficult, which means market access is hampered, which means reduced gross domestic product.
# But the reversed relationship within Africa seems puzzling. Why should difficult terrain be associated with higher GDP per capita ?

# If this relationship is at all causal, it may be because rugged regions of Africa were protected against the large-scale Atlantic and Indian Ocean slave trades.
# Slavers preferred to raid easily accessed settltments, with easy routes to the sea.
# Those resiongs that suffered under the slave trade understandably continue to suffer economically, long after the decline of slave-trading markets.


