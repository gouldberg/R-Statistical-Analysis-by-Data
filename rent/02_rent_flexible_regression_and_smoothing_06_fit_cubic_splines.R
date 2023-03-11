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
# Fit P-splines, Cubic-splines
#   - While in P-splines we take equidistant knots in the x axis, in cubic smoothing splines the knots are at the distinct x-variable values
#   - P-splines achieve smoothness in the fitted function by penalizing the parameters. Cubic smoothing splines achieve smoothness by penalizing the second
#     derivative of the function
#   - The resulting smoothing curves are usually very similar for the same effective degrees of freedom
# ------------------------------------------------------------------------------

rps <- gamlss(R ~ pb(Fl) + pb(A) + H + loc, family = GA, data = rent)

rcs <- gamlss(R ~ cs(Fl) + cs(A) + H + loc, family = GA, data = rent)



# ----------
# cs() by default will use 3 extra degrees of freedom for smoothing for x (5 in total, if you include the linear and constant)
# scs() by default will estimate lambda (and therefore the degrees of freedom) automaticlaly using generalized cross validation (GCV)

rscs <- gamlss(R ~ scs(Fl) + scs(A) + H + loc, family = GA, data = rent)




# ----------
GAIC(rps, rcs, rscs)


# -->
# Almost same



# ------------------------------------------------------------------------------
# Plot of the fitted terms
# ------------------------------------------------------------------------------

term.plot(rps, pages = 1, ask = FALSE)

term.plot(rcs, pages = 1, ask = FALSE)

term.plot(rscs, pages = 1, ask = FALSE)


