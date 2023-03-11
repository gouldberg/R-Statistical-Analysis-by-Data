setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)


s

# ------------------------------------------------------------------------------
# Tweedie GLM
#   - Y = X1 + X2 + X3 + ... + Xn
#     Xi ~ Gamma:  shape = (2-p)/(p-1)   scale = phi * (p-1) * mu^(p-1)
#     N ~ Poisson:  mean of Poisson is mu^(2-p) / ((2-p)*phi)
#
#   - The interesting feature of this distribution is the possibility that N = 0 and so that Y = 0.
#     This is useful for modeling responses where there is a nonzero probability that Y = 0 but is otherwise a postive value.
# ------------------------------------------------------------------------------

library(mgcv)


# It seems reasonable that the predictors would have a multiplicative rather than additive effect on the mean response
# In the standard linear model, this would be achieved by logging the response, but that is not possible when some response values are zero.
# We avoid that obstable by using the log in the link.

twmod <- gam(involact ~ race + fire + theft + age + log(income), family = tw(link = "log"), data = chredlin)

summary(twmod)

twmod$fit




# ------------------------------------------------------------------------------
# Predicted distribution for the first zip code
# ------------------------------------------------------------------------------

xgrid <- seq(1e-10, 1.25, len = 100)

p <- 1.108

phi <- 0.30847


# expexted mean response
( mu <- twmod$fit[1] )


# the mean of Poisson
( poismean <- mu ^ (2-p) / (phi * (2 - p)) )


# probability of a zero response
( p0 <- exp(-poismean) )



# ----------
# Now we compute the density for the nonzero part of the response, scale it down for the probability of a nonzero response and plot.

# ldTweedie:  A function to evaluate the log of the Tweedie density for variance powers between 1 and 2, inclusive.
# A matrix with 6 columns or 10 if all.derivs = TRUE, is returned.
# 1st:  the log density of y
# 2nd and 3rd: the 1st and 2nd derivatives of the log density w.r.t. phi,
# 4th and 5th columns are 1st and 2nd derivative w.r.t. p
# 6th is 2nd derivative w.r.t phi and p.

( twden <- exp(ldTweedie(xgrid, mu, p = p, phi = phi)[,1]) )

( dmax <- max(twden * (1 - p0)) )

plot(xgrid, twden * (1 - p0), type = "l", xlab = "x", ylab = "Density")
segments(0, 0, 0, dmax, lwd = 2)
text(0.05, dmax, paste0("p=", signif(p0, 3)))


