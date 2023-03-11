setwd("//media//kswada//MyFiles//R//rent")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rent
# ------------------------------------------------------------------------------
data("rent", package = "gamlss.data")


str(rent)

car::some(rent)



# ------------------------------------------------------------------------------
# Creating a new factor
# ------------------------------------------------------------------------------

# creating a new factor with levels
rent <- transform(rent, cA = cut(rent$A, c(1880, seq(1900, 1990, 10)), labels = as.character(seq(1890, 1980, 10))))


nlevels(rent$cA)


plot(R ~ cA, data = rent)



# ------------------------------------------------------------------------------
# Model by reducing levels of factor
# ------------------------------------------------------------------------------

# Null model
rn <- gamlss(R ~ 1, data = rent, family = GA)



# Saturated model, i.e. cA as a factor
rs <- gamlss(R ~ cA, data = rent, family = GA)



# penalized cA by selecting the smoothing parameter using local maximum likelihood (ML)
r1 <- gamlss(R ~ pcat(cA), data = rent, family = GA)



# penalized cA where the smoothing parameter is selected using a local GAIC
r2 <- gamlss(R ~ pcat(cA, method = "GAIC", k = log(1968)), data = rent, family = GA)



# ----------
GAIC(rn, rs, r1, r2)


# -->
# By using pcat() we improve the fit as judged by the GAIC.



# ----------
# Amount of smoothing required by getting the effective degrees of freedom
r1$mu.df
r2$mu.df


# -->
# It appears that we can move from 10 degrees of freedom (saturated model) to 3 degrees of freedom without a great loss of information.



# ------------------------------------------------------------------------------
# Obtain new factor
# ------------------------------------------------------------------------------
# pcat() automatically saves a new factor with the new classification of the original levels
# obtain the new factor

( f1 <- getSmo(r1)$factor )

levels(f1)

table(f1, rent$cA)



# -->
# we see that the levels of the new factor f1 correspond to the periods: up to 1960, 1970 and 1980



# ------------------------------------------------------------------------------
# Refit by the new factor
# ------------------------------------------------------------------------------

r3 <- gamlss(R ~ f1, data = rent, family = GA)

summary(r3)


AIC(r1, r3)



# ------------------------------------------------------------------------------
# PlotLambda() to understand how pcat() works, plotting the path of the merging levels agains log lambda
# PlotDF, plotting the path of the merging levels agains the effective degrees of freedom
# ------------------------------------------------------------------------------

# IT TAKES TIME !!!
plotLambda(R, factor = cA, data = rent, family = GA, along = seq(-10, 2, .2))


abline(v = log(getSmo(r1)$lambda), col = "gray", lwd = 2.5)
abline(v = log(getSmo(r2)$lambda), col = "darkblue", lty = 3, lwd = 2.5)



# ----------
plotDF(R, factor = cA, data = rent, family = GA)

abline(v = getSmo(r1)$edf, col = "gray", lwd = 2.5)
abline(v = getSmo(r2)$edf, col = "darkblue", lty = 3, lwd = 2.5)



# ------------------------------------------------------------------------------
# Model by reducing levels of factor, with explanatory variables in the model
# ------------------------------------------------------------------------------

rs <- gamlss(R ~ pb(Fl) + cA + B + H + loc, data = rent, family = GA)

r1 <- gamlss(R ~ pb(Fl) + pcat(cA) + B + H + loc, data = rent, family = GA)


AIC(rs, r1)



# ----------
getSmo(r1, which = 2)$edf


nf <- getSmo(r1, which = 2)$factor

levels(nf)



# ----------
r2 <- gamlss(R ~ pb(Fl) + nf + B + H + loc, data = rent, family = GA)

AIC(r1, r2)



# ----------
plotDF(formula = R ~ pb(Fl) + B + H + loc, factor = cA, data = rent, family = GA)

abline(v = getSmo(r1, which = 2)$edf, col = "gray", lwd = 2.5)

