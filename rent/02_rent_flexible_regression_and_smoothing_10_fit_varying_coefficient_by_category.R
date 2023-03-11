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
# Model varying coefficients interaction by each category
#   - When the z variable used in the argument "by" is a factor, the varying coefficient function fits separate smooth curves
#     against x for each elvel of the factor z.
# ------------------------------------------------------------------------------

g1 <- gamlss(R ~ pb(Fl) + pb(A) + pvc(Fl, by = loc), data = rent, family = GA)


g2 <- gamlss(R ~ pb(Fl) + pb(A) + pvc(A, by = loc), data = rent, family = GA)


g3 <- gamlss(R ~ pb(Fl) + pb(A) + pvc(Fl, by = loc) + pvc(A, by = loc), data = rent, family = GA)



# ----------
AIC(g1, g2, g3)



# ----------
term.plot(g1, pages = 1, ask = FALSE)


# -->
# The 3 different smooth fits (for the three levels of loc) are cramped within the third panel 


# individual plot
# plot(getSmo(g1, para = "mu", which = 1), factor.plots = TRUE)
# plot(getSmo(g1, para = "mu", which = 2), factor.plots = TRUE)
plot(getSmo(g1, para = "mu", which = 3), factor.plots = TRUE)



# ----------
# The 3 fits appear linear, so a linear interaction Fl * loc may be adequate
g4 <- gamlss(R ~ pb(Fl) + pb(A) + Fl * loc, data = rent, family = GA)


AIC(g4)




