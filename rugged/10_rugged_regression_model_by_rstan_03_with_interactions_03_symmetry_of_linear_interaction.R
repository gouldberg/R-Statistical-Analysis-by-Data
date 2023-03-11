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
# regression modeling with interactions in different way
#   - 1. the influence of ruggedness depends upon continent  (DONE)
#   - 2. the influence of continent depends upon ruggedness (we try here)
# ------------------------------------------------------------------------------

q.rugged <- range(dd$rugged)


mu.ruggedlo <- link(mod5, data = data.frame(rugged = q.rugged[1], cont_africa = 0:1))

mu.ruggedlo.mean <- apply(mu.ruggedlo, 2, mean)

mu.ruggedlo.PI <- apply(mu.ruggedlo, 2, PI)



# ----------
mu.ruggedhi <- link(mod5, data = data.frame(rugged = q.rugged[2], cont_africa = 0:1))

mu.ruggedhi.mean <- apply(mu.ruggedhi, 2, mean)

mu.ruggedhi.PI <- apply(mu.ruggedhi, 2, PI)



# ----------
# plot it all, splitting points at median
par(mfrow=c(1,1))

med.r <- median(dd$rugged)

ox <- ifelse(dd$rugged > med.r, 0.05, -0.05)



plot(dd$cont_africa + ox, log(dd$rgdppc_2000), col = ifelse(dd$rugged>med.r, rangi2, "black"), xlim = c(-0.25, 1.25), xaxt = "n", ylab = "log GDP year 2000", xlab = "Continent")

axis(1, at = c(0,1), labels = c("other", "africa"))

lines(0:1, mu.ruggedlo.mean, lty = 2)

shade(mu.ruggedlo.PI, 0:1)

lines(0:1, mu.ruggedhi.mean, col = rangi2)

shade(mu.ruggedhi.PI, 0:1, col = col.alpha(rangi2, 0.25))



# -->
# Black points: nations with terrain ruggedness below the median.
# Blue points: nations with terrain ruggedness above the median
# horizontal axis: continent
# Black dashed regression line and shaded confidence region: the expected reduction in log-GDP when we take a nation with minimum terrain ruggedness (0.003) and change its continent
# Blue regression line and shaded region: the expected change for an imaginary nation with maximum observed terrain ruggedness (6.2)

# being Africa is on average bad for GDP.
# Changing continent has almost no expected effect -- the line does slope upwards a tiny amount, but the wide shaded interval should prevent us from
# getting excited abou that fact.
# For a nation with very high ruggedness, there is almost no negative effect on GDP of being in Africa.

# In previous analysis, it was not obvious that African nations are on average nearly always worse off.
# It's just at very high value of rugged that nations inside and outside of Africa have the same expected log-GDP.
# This 2nd way of plotting the interaction makes this clear.

