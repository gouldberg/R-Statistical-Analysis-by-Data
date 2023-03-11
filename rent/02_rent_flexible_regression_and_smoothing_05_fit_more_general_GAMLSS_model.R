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
# 3-parameter Box-Cox Cole and Green (BCCGo) distribution
# ------------------------------------------------------------------------------

# available links within the BCCGo distribution.
show.link(BCCGo)


# ----------
r6 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc, sigma.fo = ~pb(Fl) + pb(A) + H + loc, nu.fo = ~1, family = BCCGo, data = rent)


r7 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc, sigma.fo = ~pb(Fl) + pb(A) + H + loc, nu.fo = ~pb(Fl) + pb(A) + H + loc, family = BCCGo, data = rent)


AIC(r4, r6, r7)



# ----------
# Check the adequacy of the fitted distributions
wp(r6, ylim.all = 0.6);  title("r6: BCCG(mu, sigma)")

wp(r7, ylim.all = 0.6);  title("r7: BCCG(mu, sigma, nu)")

