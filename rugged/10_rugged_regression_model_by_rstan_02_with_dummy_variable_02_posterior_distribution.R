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
# plot posterior distribution for model 3
# ------------------------------------------------------------------------------

# plot model 3 (entire data in one model)
rugged.seq <- 0:7


pred.data <- data.frame(rugged = rugged.seq)



mu3 <- link(mod3, data = pred.data, n = 1e4)

mu3.mean <- apply(mu3, 2, mean)

mu3.PI <- apply(mu3, 2, PI)



# ----------
par(mfrow=c(1,1))
plot(log_gdp ~ rugged, data = dd, col = rangi2)

mtext("Africa + Non Africa")

lines(rugged.seq, mu3.mean)

shade(mu3.PI, rugged.seq)




# ------------------------------------------------------------------------------
# plot posterior distribution for model 4
# ------------------------------------------------------------------------------

# plot posterior distribution for model 4
rugged.eq <- seq(from = -1, to = 8, by = 0.25)


mu.NotAfrica <- link(mod4, data = data.frame(cont_africa = 0, rugged = rugged.seq))

mu.Africa <- link(mod4, data = data.frame(cont_africa = 1, rugged = rugged.seq))



# ----------
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)

mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI, prob = 0.97)

mu.Africa.mean <- apply(mu.Africa, 2, mean)

mu.Africa.PI <- apply(mu.Africa, 2, PI, prob = 0.97)



# ----------
par(mfrow=c(1,1))

plot(log_gdp ~ rugged, type = "n", data = dd)

points(d.A0$rugged, d.A0$log_gdp, col = "blue")

points(d.A1$rugged, d.A1$log_gdp, col = "red")

mtext("Africa + Non Africa")

lines(rugged.seq, mu.NotAfrica.mean, col = "blue")

shade(mu.NotAfrica.PI, rugged.seq, col = col.alpha("blue", 0.15))

lines(rugged.seq, mu.Africa.mean, col = "red")

shade(mu.Africa.PI, rugged.seq, col = col.alpha("red", 0.15))



# -->
# Rather weak negative relationship between economic development and ruggedness.
# The African nations do have lower overall economic development, and so the regression line is below, but parallel to non-african
# It can not do anything to the slope of the line.

# The fact that WAIC tells you that the model with the dummy variable is hugely better only indicates that African nations on average do have lower GDP.

